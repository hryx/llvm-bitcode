//! Handles reading and writing LLVM bitcode files.

const std = @import("std");
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

pub fn Parser(comptime ReaderType: type) type {
    return struct {
        arena: ArenaAllocator,
        bitstream_reader: bitstream.Reader(ReaderType),
        abbrev_id_width: u32,
        found_identification: bool,
        found_module: bool,

        const Self = @This();

        pub fn init(gpa: Allocator, reader: ReaderType) Self {
            return Self{
                .arena = ArenaAllocator.init(gpa),
                .bitstream_reader = bitstream.reader(reader),
                .abbrev_id_width = 2,
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
            const file_magic = try self.bitstream_reader.readMagic();
            if (!std.mem.eql(u8, &file_magic, &Bitcode.magic)) {
                return ParseResult{ .failure = try self.parseError("bad magic", .{}) };
            }

            var bc = Bitcode{
                .identification = undefined,
                .module = undefined,
                .symtab = undefined,
                .strtab = undefined,
            };

            blocks: while (true) {
                const abbrev_id = self.bitstream_reader.readAbbreviationId(2) catch |err| switch (err) {
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
                .BLOCKINFO => {
                    var block_info = BlockInfo{};
                    if (try self.parseBlockInfo(&block_info)) |fail| {
                        return fail;
                    }
                    std.log.info("blockinfo: {any}", .{block_info});
                },
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

        const BlockInfo = struct {
            items: []Item = &.{},

            const Item = struct {
                id: u32,
                abbrevs: []void, // TODO
                name: []const u8,
                record_names: std.AutoHashMapUnmanaged(u32, []const u8),
            };
        };

        fn parseBlockInfo(self: *Self, bi: *BlockInfo) !?ParseError {
            records: while (true) {
                const id = try self.bitstream_reader.readAbbreviationId(self.abbrev_id_width);
                switch (id) {
                    .END_BLOCK => return null,
                    .ENTER_SUBBLOCK => return try self.parseError("found ENTER_SUBBLOCK in BLOCKINFO", .{}),
                    .DEFINE_ABBREV => {
                        @panic("TODO: define abbrev in BLOCKINFO");
                    },
                    .UNABBREV_RECORD => return try self.parseError("found UNABBREV_RECORD in BLOCKINFO", .{}),
                    _ => {},
                }
                std.debug.panic("TODO: found BLOCKINFO abbrev {}", .{id});
                if (false) break :records;
            }
            _ = bi;
        }

        fn parseModuleBlock(self: *Self, bc: *Bitcode) !?ParseError {
            if (self.found_module) {
                return try self.parseError("duplicate module block", .{});
            }
            self.found_module = true;
            while (true) {
                const id = try self.bitstream_reader.readAbbreviationId(self.abbrev_id_width);
                switch (id) {
                    .END_BLOCK => {
                        try self.bitstream_reader.alignToWord();
                        return null;
                    },
                    .ENTER_SUBBLOCK => {
                        if (try self.parseSubBlock(bc)) |fail| {
                            return fail;
                        }
                    },
                    .DEFINE_ABBREV => {
                        @panic("TODO: parse define abbrev");
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
                                const op = try self.bitstream_reader.readVbr(u32, 6);
                                if (op != 2) {
                                    return try self.parseError("only MODULE_CODE_VERSION 2 is supported, got {}", .{op});
                                }
                                bc.module.version = @intCast(u2, op);
                            },
                            .MODULE_CODE_TRIPLE,
                            .MODULE_CODE_DATALAYOUT,
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
                        std.debug.panic("TODO: parse abbrev id {}", .{id});
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
            const id = try self.bitstream_reader.readAbbreviationId(self.abbrev_id_width);
            _ = id;
            _ = bc;
            // TODO: bc.identification = x;
            return null;
        }

        fn parseVbrSlice(self: *Self, comptime T: type, len: u32) ![]T {
            var list = ArrayList(T).init(self.arena.allocator());
            try list.ensureTotalCapacity(len);
            var i: u32 = 0;
            while (i < len) : (i += 1) {
                const op = try self.bitstream_reader.readVbr(u32, 6);
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
