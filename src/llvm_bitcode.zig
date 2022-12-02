//! Handles reading and writing LLVM bitcode files.

const std = @import("std");
const assert = std.debug.assert;
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;
const ArenaAllocator = std.heap.ArenaAllocator;

const bitstream = @import("bitstream.zig");
const Bitcode = @import("Bitcode.zig");

pub const ParseResult = union(enum) {
    success: Bitcode,
    failure: Error,

    pub const Error = struct {
        msg: []const u8,
        pos: usize,
    };
};

const ParseError = ParseResult.Error;

const BlockInfo = struct {
    abbrevs: std.ArrayListUnmanaged(Abbrev) = .{},
    name: ?[]const u8 = null,
    record_names: std.AutoHashMapUnmanaged(u32, []const u8) = .{},
};

const Abbrev = struct {
    id_encoding: bitstream.AbbrevDefOp,
    op_encoding: []bitstream.AbbrevDefOp,
};

pub fn Parser(comptime ReaderType: type) type {
    return struct {
        arena: ArenaAllocator,
        bitstream_reader: bitstream.Reader(ReaderType),
        abbrev_id_width: u32,
        block_info: [block_info_len]BlockInfo,
        found_identification: bool,
        found_module: bool,

        const block_info_len = Bitcode.BlockId.last_known_block_id + 1;
        const Self = @This();

        pub fn init(gpa: Allocator, reader: ReaderType) Self {
            return Self{
                .arena = ArenaAllocator.init(gpa),
                .bitstream_reader = bitstream.reader(reader),
                .abbrev_id_width = 2,
                .block_info = [1]BlockInfo{.{}} ** block_info_len,
                .found_identification = false,
                .found_module = false,
            };
        }

        pub fn deinit(self: *Self) void {
            self.arena.deinit();
        }

        fn parseError(self: *Self, comptime fmt: []const u8, args: anytype) !ParseError {
            const msg = try std.fmt.allocPrint(self.arena.allocator(), fmt, args);
            return ParseError{ .msg = msg, .pos = self.bitstream_reader.pos };
        }

        pub fn parse(self: *Self) !ParseResult {
            return self.parseInner() catch |err| switch (err) {
                error.EndOfStream => ParseResult{ .failure = try self.parseError("unexpected end of bitstream", .{}) },
                error.InvalidBitstream => ParseResult{ .failure = try self.parseError("invalid bitstream", .{}) },
                else => err,
            };
        }

        fn parseInner(self: *Self) !ParseResult {
            const file_magic = try self.bitstream_reader.readMagic();
            if (!std.mem.eql(u8, &file_magic, &Bitcode.magic)) {
                return ParseResult{ .failure = try self.parseError("bad magic", .{}) };
            }

            var bc: Bitcode = undefined;

            blocks: while (true) {
                const abbrev_id = self.readAbbrevId() catch |err| switch (err) {
                    error.EndOfStream => {
                        if (self.bitstream_reader.pos % 32 != 0) {
                            return err;
                        }
                        break :blocks;
                    },
                    else => return err,
                };
                if (abbrev_id != .ENTER_SUBBLOCK) {
                    return ParseResult{ .failure = try self.parseError("expected ENTER_SUBBLOCK at top level", .{}) };
                }

                if (try self.parseSubBlock(&bc)) |fail| {
                    return ParseResult{ .failure = fail };
                }
            }

            return ParseResult{ .success = bc };
        }

        fn parseSubBlock(self: *Self, bc: *Bitcode) !?ParseError {
            const header = try self.bitstream_reader.readSubBlockHeader();
            std.log.info("got header block: {any}", .{header});

            const prev_abbr_width = self.abbrev_id_width;
            defer self.abbrev_id_width = prev_abbr_width;
            self.abbrev_id_width = header.new_abbr_id_width;

            switch (header.id) {
                .BLOCKINFO => return try self.parseBlockInfo(),
                _ => {},
            }

            const id = @intToEnum(Bitcode.BlockId, @enumToInt(header.id));
            switch (id) {
                .PARAMATTR_BLOCK_ID,
                .PARAMATTR_GROUP_BLOCK_ID,
                .CONSTANTS_BLOCK_ID,
                .FUNCTION_BLOCK_ID,
                .IDENTIFICATION_BLOCK_ID,
                .VALUE_SYMTAB_BLOCK_ID,
                .METADATA_BLOCK_ID,
                .METADATA_ATTACHMENT_ID,
                .TYPE_BLOCK_ID_NEW,
                .USELIST_BLOCK_ID,
                .MODULE_STRTAB_BLOCK_ID,
                .GLOBALVAL_SUMMARY_BLOCK_ID,
                .OPERAND_BUNDLE_TAGS_BLOCK_ID,
                .METADATA_KIND_BLOCK_ID,
                .STRTAB_BLOCK_ID,
                .FULL_LTO_GLOBALVAL_SUMMARY_BLOCK_ID,
                .SYMTAB_BLOCK_ID,
                .SYNC_SCOPE_NAMES_BLOCK_ID,
                => {
                    std.log.info("TODO: parse block {s}", .{@tagName(id)});
                    try self.bitstream_reader.skipWords(header.word_count);
                },
                .MODULE_BLOCK_ID => return self.parseModuleBlock(bc),
                _ => return try self.parseError("unknown block ID {}", .{@enumToInt(header.id)}),
            }
            return null;
        }

        fn parseBlockInfo(self: *Self) !?ParseError {
            var block_id: ?Bitcode.BlockId = null;
            while (true) {
                const id = try self.readAbbrevId();
                std.log.info("BLOCKINFO record code {}", .{id});
                switch (id) {
                    .END_BLOCK => {
                        try self.bitstream_reader.endBlock();
                        return null;
                    },
                    .ENTER_SUBBLOCK => return try self.parseError("found ENTER_SUBBLOCK in BLOCKINFO", .{}),
                    .DEFINE_ABBREV => {
                        if (block_id == null) {
                            return try self.parseError("found DEFINE_ABBREV before SETBID in BLOCKINFO", .{});
                        }
                        if (try self.defineAbbrev(block_id.?)) |fail| return fail;
                    },
                    .UNABBREV_RECORD => {
                        const code = try self.bitstream_reader.readVbr(u32, 6);
                        const length = try self.bitstream_reader.readVbr(u32, 6);
                        std.log.info("unabbrev: code {} len {}", .{ code, length });
                        switch (@intToEnum(bitstream.BlockInfoCode, code)) {
                            .BLOCKINFO_CODE_SETBID => {
                                if (length != 1) {
                                    return try self.parseError("expected one arg to SETBID, got {}", .{length});
                                }
                                block_id = @intToEnum(Bitcode.BlockId, try self.bitstream_reader.readVbr(u32, 6));
                                std.log.info("SETBID {}", .{block_id.?});
                            },
                            .BLOCKINFO_CODE_BLOCKNAME => @panic("TODO BLOCKNAME"),
                            .BLOCKINFO_CODE_SETRECORDNAME => @panic("TODO SETRECORDNAME"),
                            _ => return try self.parseError("unknown BLOCKINFO code {}", .{code}),
                        }
                    },
                    _ => return try self.parseError("unknown abbreviation id {} in BLOCKINFO", .{@enumToInt(id)}),
                }
            }
        }

        fn parseModuleBlock(self: *Self, bc: *Bitcode) !?ParseError {
            if (self.found_module) {
                return try self.parseError("duplicate module block", .{});
            }
            self.found_module = true;
            while (true) {
                const id = try self.readAbbrevId();
                switch (id) {
                    .END_BLOCK => {
                        try self.bitstream_reader.endBlock();
                        return null;
                    },
                    .ENTER_SUBBLOCK => {
                        if (try self.parseSubBlock(bc)) |fail| {
                            return fail;
                        }
                    },
                    .DEFINE_ABBREV => {
                        if (try self.defineAbbrev(.MODULE_BLOCK_ID)) |fail| {
                            return fail;
                        }
                    },
                    .UNABBREV_RECORD => {
                        const code = try self.bitstream_reader.readVbr(u32, 6);
                        const length = try self.bitstream_reader.readVbr(u32, 6);
                        std.log.info("unabbrev: code {} len {}", .{ code, length });
                        switch (@intToEnum(Bitcode.Module.Code, code)) {
                            .MODULE_CODE_VERSION => {
                                if (length != 1) {
                                    return try self.parseError("MODULE_CODE_VERSION expected one op, got {}", .{length});
                                }
                                const version = try self.bitstream_reader.readVbr(u32, 6);
                                if (version != 2) {
                                    return try self.parseError("only MODULE_CODE_VERSION 2 is supported, got {}", .{version});
                                }
                                bc.module.version = @intCast(u2, version);
                            },
                            .MODULE_CODE_TRIPLE => {
                                bc.module.triple = try self.parseUnabbrevOps(u8, length);
                            },
                            .MODULE_CODE_DATALAYOUT => {
                                bc.module.data_layout = try self.parseUnabbrevOps(u8, length);
                            },
                            .MODULE_CODE_SOURCE_FILENAME => {
                                bc.module.source_filename = try self.parseUnabbrevOps(u8, length);
                            },
                            .MODULE_CODE_ASM,
                            .MODULE_CODE_SECTIONNAME,
                            .MODULE_CODE_DEPLIB,
                            .MODULE_CODE_GLOBALVAR,
                            .MODULE_CODE_FUNCTION,
                            .MODULE_CODE_ALIAS,
                            .MODULE_CODE_GCNAME,
                            => |c| {
                                std.debug.panic("TODO: handle module code {s}", .{@tagName(c)});
                            },
                            _ => {
                                return try self.parseError("unexpected abbrev code {} in module block", .{code});
                            },
                        }
                    },
                    _ => {
                        // Custom defined abbreviation
                        const abbr_id = @enumToInt(id);
                        const abbr = self.getAbbrev(.MODULE_BLOCK_ID, abbr_id) orelse {
                            return try self.parseError("abbrev {} not yet defined for module block", .{abbr_id});
                        };
                        const code_id = @intToEnum(Bitcode.Module.Code, try self.parseAbbrevRecordCode(abbr));
                        std.log.info("  module block: got abbreviated code for {}", .{code_id});
                        switch (code_id) {
                            .MODULE_CODE_SOURCE_FILENAME => {
                                const read = try self.parseAbbrevVal(abbr.op_encoding, &bc.module.source_filename);
                                assert(read == 2);
                                std.log.info("got source filename: {s}", .{bc.module.source_filename});
                            },
                            .MODULE_CODE_VERSION,
                            .MODULE_CODE_TRIPLE,
                            .MODULE_CODE_DATALAYOUT,
                            .MODULE_CODE_ASM,
                            .MODULE_CODE_SECTIONNAME,
                            .MODULE_CODE_DEPLIB,
                            .MODULE_CODE_GLOBALVAR,
                            .MODULE_CODE_FUNCTION,
                            .MODULE_CODE_ALIAS,
                            .MODULE_CODE_GCNAME,
                            => {
                                std.debug.panic("TODO: parse abbreviated module code {}", .{code_id});
                            },
                            _ => {
                                return try self.parseError("unknown module code {} in abbrev record", .{@enumToInt(code_id)});
                            },
                        }
                        // try self.parseWithAbbrev(abbr);
                        // std.log.info("TODO: found abbr {}: {any}", .{ abbr_id, abbr });
                    },
                }
            }

            return null;
        }

        fn parseIdentificationBlock(self: *Self, bc: *Bitcode) !?ParseError {
            if (self.found_identification) {
                return try self.parseError("duplicate IDENTIFICATION block", .{});
            }
            self.found_identification = true;
            const id = try self.readAbbrevId();
            _ = id;
            _ = bc;
            // TODO: bc.identification = x;
            return null;
        }

        fn readAbbrevId(self: *Self) !bitstream.AbbreviationId {
            return self.bitstream_reader.readAbbreviationId(self.abbrev_id_width);
        }

        /// Parse bits during DEFINE_ABBREV, storing the abbreviation in
        /// the given block. An abbrev's ID is based on its index when appended.
        fn defineAbbrev(self: *Self, block_id: Bitcode.BlockId) !?ParseError {
            const index = @enumToInt(block_id);
            assert(index < block_info_len);
            const abbrevs = &self.block_info[index].abbrevs;

            const op_count = try self.bitstream_reader.readVbr(u32, 5);
            if (op_count == 0) {
                return try self.parseError("empty op list in abbrev", .{});
            }

            const id_encoding = try self.bitstream_reader.readAbbrevDefOp();
            switch (id_encoding) {
                .literal => {},
                .encoded => |enc| switch (enc) {
                    .fixed, .vbr, .char6 => {},
                    .array => return try self.parseError("abbrev record code cannot be encoded as array", .{}),
                    .blob => return try self.parseError("abbrev record code cannot be encoded as blob", .{}),
                },
            }
            var abbrev = Abbrev{
                .id_encoding = id_encoding,
                .op_encoding = &.{},
            };
            std.log.info("DEFINE_ABBREV block {} num ops {}", .{ block_id, op_count });

            const arg_count = op_count - 1;
            if (arg_count > 0) {
                const ops = try self.arena.allocator().alloc(bitstream.AbbrevDefOp, arg_count);

                var i: u32 = 0;
                var last_op_was_array = false;
                while (i < arg_count) : (i += 1) {
                    const op = try self.bitstream_reader.readAbbrevDefOp();
                    std.log.info("  ABBREV OP: {any}", .{op});
                    last_op_was_array = op == .encoded and op.encoded == .array;
                    ops[i] = op;
                }
                if (last_op_was_array) {
                    return try self.parseError("abbrev def array op was not followed by type op", .{});
                }

                abbrev.op_encoding = ops;
            }

            try abbrevs.append(self.arena.allocator(), abbrev);
            return null;
        }

        /// Returns an abbrev encoding previously defined with DEFINE_ABBREV
        /// for the given block, or null if the abbrev ID has not been defined.
        fn getAbbrev(self: *Self, block_id: Bitcode.BlockId, abbr_id: u32) ?Abbrev {
            const first_id = bitstream.AbbreviationId.first_application_abbrev_id;
            assert(abbr_id >= first_id);
            const adjusted_id = abbr_id - first_id;

            const abbrs = self.block_info[@enumToInt(block_id)].abbrevs.items;
            if (adjusted_id > abbrs.len) {
                return null;
            }
            return abbrs[adjusted_id];
        }

        /// When referencing an abbreviated record, call this to get the record code.
        /// For non-literal code IDs, this reads more bits.
        fn parseAbbrevRecordCode(self: *Self, abbr: Abbrev) !u32 {
            return switch (abbr.id_encoding) {
                .literal => |lit| @intCast(u32, lit), // TODO cast is temp hack
                .encoded => |enc| switch (enc) {
                    .fixed => |width| try self.bitstream_reader.readInt(u32, width),
                    .vbr => |width| try self.bitstream_reader.readVbr(u32, width),
                    .char6 => try self.bitstream_reader.readChar6(),
                    .array, .blob => unreachable, // rejected during DEFINE_ABBREV
                },
            };
        }

        /// Parse a single value into `dst` using the encoding rules stored in `abbrev_ops`.
        /// `dst` must be a pointer type and be compatible with the types described by
        /// the encoding rule in `abbrev_ops`.
        /// Data encoded as an array expects `dst` to be a `*[]T`.
        /// This function allocates the slice.
        ///
        /// Returns the number of ops consumed, which will be more than one for array ops.
        ///
        /// The `abbrev_ops` slice should start at the current op position,
        /// so subsequent calls should pass a subslice whose start is the total number
        /// of ops read so far, i.e `ops[read_so_far..]`.
        fn parseAbbrevVal(self: *Self, abbrev_ops: []bitstream.AbbrevDefOp, dst: anytype) !u16 {
            assert(abbrev_ops.len > 0);

            const Ptr = @TypeOf(dst);
            const ptr_info = @typeInfo(Ptr);
            if (ptr_info != .Pointer or ptr_info.Pointer.size != .One) {
                @compileError("dst must be a *T");
            }
            const Dst = ptr_info.Pointer.child;
            const dst_info = @typeInfo(Dst);

            switch (dst_info) {
                .Int => {
                    switch (abbrev_ops[0]) {
                        .literal => |lit| {
                            dst.* = @intCast(Dst, lit);
                        },
                        .encoded => |enc| switch (enc) {
                            .fixed => |width| {
                                dst.* = try self.bitstream_reader.readInt(Dst, width);
                            },
                            .vbr => |width| {
                                dst.* = try self.bitstream_reader.readVbr(Dst, width);
                            },
                            .char6 => {
                                dst.* = @intCast(Dst, try self.bitstream_reader.readChar6());
                            },
                            inline else => |e| {
                                return try self.parseError("expected int type for abbrev op, got {s}", .{@tagName(e)});
                            },
                        },
                    }
                    return 1;
                },
                .Pointer => |p| {
                    if (p.size != .Slice) {
                        @compileError("can only parse abbrev vals into int or slice");
                    }
                    const Elem = p.child;
                    if (@typeInfo(Elem) != .Int) {
                        @compileError("slice element only supports ints for now");
                    }

                    var elem_type = abbrev_ops[1];
                    if (!(elem_type == .encoded and elem_type.encoded == .char6)) {
                        std.debug.panic("TODO: parse arrays other than []char6, got {}", .{elem_type});
                    }

                    const len = try self.bitstream_reader.readVbr(usize, 6);
                    if (len == 0) return 2;

                    const slice = try self.arena.allocator().alloc(Elem, len);
                    var i: usize = 0;
                    while (i < len) : (i += 1) {
                        slice[i] = try self.bitstream_reader.readChar6();
                    }
                    dst.* = slice;

                    return 2;
                },
                else => @compileError("can only parse abbrev vals into int or slice"),
            }
        }

        fn parseUnabbrevOps(self: *Self, comptime T: type, count: u32) ![]T {
            return self.parseVbrSlice(T, 6, count);
        }

        fn parseVbrSlice(self: *Self, comptime T: type, width: u16, count: u32) ![]T {
            var list = ArrayList(T).init(self.arena.allocator());
            try list.ensureTotalCapacity(count);
            var i: u32 = 0;
            while (i < count) : (i += 1) {
                const op = try self.bitstream_reader.readVbr(u32, width);
                if (op > std.math.maxInt(T)) {
                    return error.TODO;
                }
                list.appendAssumeCapacity(@intCast(T, op));
            }
            return list.toOwnedSlice();
        }
    };
}

pub fn parser(gpa: Allocator, reader: anytype) Parser(@TypeOf(reader)) {
    return Parser(@TypeOf(reader)).init(gpa, reader);
}

pub fn dump(gpa: Allocator, r: anytype) !void {
    var p = parser(gpa, r);
    const res = try p.parse();
    switch (res) {
        .success => std.log.info("TODO: dump pared bitcode", .{}),
        .failure => |err| {
            const byte = err.pos / 8;
            const bit_off = err.pos % 8;
            std.log.err(
                "bit {} (0x{x:0>4}+{}): {s}",
                .{ err.pos, byte, bit_off, err.msg },
            );
            return error.InvalidBitcode;
        },
    }
}

test {
    _ = bitstream;
    _ = Bitcode;
}
