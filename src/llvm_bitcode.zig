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

/// Convert a Bitcode block ID into an index into the BlockInfo array.
/// Asserts the block ID is known.
fn blockInfoIndex(id: Bitcode.BlockId) u32 {
    const idx = @enumToInt(id);
    assert(idx >= bitstream.BlockId.first_application_block_id);
    assert(idx <= @enumToInt(Bitcode.BlockId.last_known_block_id));
    return idx - bitstream.BlockId.first_application_block_id;
}

/// Convert an abbreviation ID to an index in the abbreviation array.
/// Asserts the ID is not one of the standard/reserved abbreviations.
fn abbrevIdToIndex(abbr_id: bitstream.AbbreviationId) u32 {
    const first_id = bitstream.AbbreviationId.first_application_abbrev_id;
    const int = @enumToInt(abbr_id);
    assert(int >= first_id);
    return int - first_id;
}

pub fn Parser(comptime ReaderType: type) type {
    return struct {
        arena: ArenaAllocator,
        bitstream_reader: bitstream.Reader(ReaderType),
        bc: Bitcode,
        abbrev_id_width: u32,
        block_info: [block_info_len]BlockInfo,
        found_identification: bool,
        found_module: bool,
        type_entry_count: u32,
        pending_type_name: ?[]const u8,

        const block_info_len = blockInfoIndex(Bitcode.BlockId.last_known_block_id) + 1;
        const Self = @This();

        /// Asserts block ID is known.
        fn getBlockInfo(self: *Self, id: Bitcode.BlockId) *BlockInfo {
            const idx = blockInfoIndex(id);
            assert(idx < block_info_len);
            return &self.block_info[idx];
        }

        /// Returns an abbrev encoding previously defined with DEFINE_ABBREV
        /// for the given block, or null if the abbrev ID has not been defined.
        /// Asserts the block ID is a known ID and that the abbrev ID is not a reserved bitcode abbrev.
        fn getAbbrev(self: *Self, block_id: Bitcode.BlockId, abbr_id: bitstream.AbbreviationId) ?Abbrev {
            const abbrevs = self.getBlockInfo(block_id).abbrevs.items;
            const index = abbrevIdToIndex(abbr_id);
            if (index >= abbrevs.len) {
                return null;
            }
            return abbrevs[index];
        }

        pub fn init(gpa: Allocator, reader: ReaderType) Self {
            return Self{
                .arena = ArenaAllocator.init(gpa),
                .bitstream_reader = bitstream.reader(reader),
                .bc = .{},
                .abbrev_id_width = 2,
                .block_info = [1]BlockInfo{.{}} ** block_info_len,
                .found_identification = false,
                .found_module = false,
                .type_entry_count = 0,
                .pending_type_name = null,
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
                error.EndOfRecord => ParseResult{ .failure = try self.parseError("unexpected end of record", .{}) },
                error.Overflow => ParseResult{ .failure = try self.parseError("record did not fit into expected type", .{}) },
                else => err,
            };
        }

        fn parseInner(self: *Self) !ParseResult {
            const file_magic = try self.bitstream_reader.readMagic();
            if (!std.mem.eql(u8, &file_magic, &Bitcode.magic)) {
                return ParseResult{ .failure = try self.parseError("bad magic", .{}) };
            }

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

                if (try self.parseSubBlock()) |fail| {
                    return ParseResult{ .failure = fail };
                }
            }

            if (!self.found_module) {
                return ParseResult{ .failure = try self.parseError("no module block", .{}) };
            }

            if (self.type_entry_count != self.bc.module.type.entries.len) {
                return ParseResult{ .failure = try self.parseError(
                    "expected {} type table entries, found {}",
                    .{ self.bc.module.type.entries.len, self.type_entry_count },
                ) };
            }

            return ParseResult{ .success = self.bc };
        }

        fn parseSubBlock(self: *Self) !?ParseError {
            const header = try self.bitstream_reader.readSubBlockHeader();
            std.log.info("got header block: {any}", .{header});

            const prev_abbr_width = self.abbrev_id_width;
            defer self.abbrev_id_width = prev_abbr_width;
            self.abbrev_id_width = header.new_abbr_id_width;

            switch (header.id) {
                .BLOCKINFO => return try self.parseBlockInfo(),
                _ => {},
            }

            const block_id = Bitcode.BlockId.fromBitstreamBlockId(header.id);
            switch (block_id) {
                // handled below
                .MODULE_BLOCK_ID => {
                    if (self.found_module) {
                        return try self.parseError("duplicate module block", .{});
                    }
                    self.found_module = true;
                },
                .TYPE_BLOCK_ID => {
                    if (!self.found_module) {
                        return try self.parseError("type block before module block", .{});
                    }
                },

                // unhandled, skip
                .PARAMATTR_BLOCK_ID,
                .PARAMATTR_GROUP_BLOCK_ID,
                .CONSTANTS_BLOCK_ID,
                .FUNCTION_BLOCK_ID,
                .IDENTIFICATION_BLOCK_ID,
                .VALUE_SYMTAB_BLOCK_ID,
                .METADATA_BLOCK_ID,
                .METADATA_ATTACHMENT_ID,
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
                    std.log.err("TODO: parse block {s}", .{@tagName(block_id)});
                    try self.bitstream_reader.skipWords(header.word_count);
                    return null;
                },
                _ => {
                    std.log.err("unknown block ID {}", .{header.id});
                    try self.bitstream_reader.skipWords(header.word_count);
                    return null;
                },
            }

            while (true) {
                const abbrev_id = try self.readAbbrevId();
                std.log.info("  record abberev id {}", .{abbrev_id});
                switch (abbrev_id) {
                    .END_BLOCK => {
                        try self.bitstream_reader.endBlock();
                        return null;
                    },
                    .ENTER_SUBBLOCK => {
                        if (try self.parseSubBlock()) |fail| {
                            return fail;
                        }
                    },
                    .DEFINE_ABBREV => {
                        if (try self.parseDefineAbbrev(block_id)) |fail| {
                            return fail;
                        }
                    },
                    .UNABBREV_RECORD => {
                        var decoder = self.unabbrevDecoder();
                        if (try self.parseRecord(block_id, &decoder)) |fail| {
                            return fail;
                        }
                    },
                    _ => {
                        var decoder = self.abbrevDecoder(block_id, abbrev_id) orelse {
                            return try self.parseError(
                                "abbrev ID {} not registered for block {s}",
                                .{ abbrev_id, @tagName(block_id) },
                            );
                        };
                        if (try self.parseRecord(block_id, &decoder)) |fail| {
                            return fail;
                        }
                    },
                }
            }

            return null;
        }

        fn parseBlockInfo(self: *Self) !?ParseError {
            var dst_block_id: ?Bitcode.BlockId = null;
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
                        if (dst_block_id) |bid| {
                            if (try self.parseDefineAbbrev(bid)) |fail| return fail;
                        } else {
                            return try self.parseError("found DEFINE_ABBREV before SETBID in BLOCKINFO", .{});
                        }
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
                                dst_block_id = @intToEnum(Bitcode.BlockId, try self.bitstream_reader.readVbr(u32, 6));
                                std.log.info("SETBID {}", .{dst_block_id.?});
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

        fn parseRecord(self: *Self, block_id: Bitcode.BlockId, decoder: anytype) !?ParseError {
            const TODO = switch (block_id) {
                // unhandled, should have been skipped
                .PARAMATTR_BLOCK_ID,
                .PARAMATTR_GROUP_BLOCK_ID,
                .CONSTANTS_BLOCK_ID,
                .FUNCTION_BLOCK_ID,
                .IDENTIFICATION_BLOCK_ID,
                .VALUE_SYMTAB_BLOCK_ID,
                .METADATA_BLOCK_ID,
                .METADATA_ATTACHMENT_ID,
                .USELIST_BLOCK_ID,
                .MODULE_STRTAB_BLOCK_ID,
                .GLOBALVAL_SUMMARY_BLOCK_ID,
                .OPERAND_BUNDLE_TAGS_BLOCK_ID,
                .METADATA_KIND_BLOCK_ID,
                .STRTAB_BLOCK_ID,
                .FULL_LTO_GLOBALVAL_SUMMARY_BLOCK_ID,
                .SYMTAB_BLOCK_ID,
                .SYNC_SCOPE_NAMES_BLOCK_ID,
                => unreachable,
                _ => unreachable,

                .MODULE_BLOCK_ID => try self.parseModuleRecord(decoder),
                .TYPE_BLOCK_ID => try self.parseTypeRecord(decoder),
            };
            try decoder.finishOps();
            // Let's change this so parseError stores error msg instead of returning an object
            if (TODO) |xxx| return xxx;
            return null;
        }

        fn parseModuleRecord(self: *Self, decoder: anytype) !?ParseError {
            const code = try decoder.parseRecordCode(Bitcode.Module.Code);
            std.log.info("    code {}", .{code});
            switch (code) {
                .MODULE_CODE_GLOBALVAR => {
                    const G = Bitcode.Module.GlobalVar;
                    const g = G{
                        .strtab_offset = try decoder.parseOp(u32),
                        .strtab_size = try decoder.parseOp(u32),
                        .pointer_type_index = try decoder.parseOp(u32),
                        .is_const = try decoder.parseOp(bool),
                        .init_id = try decoder.parseOptionalIndex(u32),
                        .linkage = try decoder.parseOp(G.Linkage),
                        .alignment_log2 = try decoder.parseOp(u16),
                        .section_index = try decoder.parseOptionalIndex(u32),
                        .visibility = try decoder.parseOp(G.Visibility),
                        .@"threadlocal" = try decoder.parseOp(G.Threadlocal),
                        .unnamed_addr = try decoder.parseOp(G.UnnamedAddr),
                        .externally_initialized = try decoder.parseOp(bool),
                        .dll_storage_class = try decoder.parseOp(G.DllStorageClass),
                        .comdat = try decoder.skipOp(), // TODO
                        .attributes_index = try decoder.parseOptionalIndex(u32),
                        .preemption_specifier = try decoder.parseOp(G.PreemptionSpecifier),
                        // undocumented:
                        // 16 partition strtab offset
                        // 17 partition strtab size
                    };

                    const len = self.bc.module.global_var.len;
                    self.bc.module.global_var = try self.arena.allocator().realloc(self.bc.module.global_var, len + 1);
                    self.bc.module.global_var[len] = g;
                },
                .MODULE_CODE_FUNCTION => {
                    const F = Bitcode.Module.Function;
                    const G = Bitcode.Module.GlobalVar;
                    const f = F{
                        .strtab_offset = try decoder.parseOp(u32),
                        .strtab_size = try decoder.parseOp(u32),
                        .type_index = try decoder.parseOp(u32),
                        .calling_conv = try decoder.parseOp(F.CallingConv),
                        .is_proto = try decoder.parseOp(bool),
                        .linkage = try decoder.parseOp(G.Linkage),
                        .param_attr_index = try decoder.parseOptionalIndex(u32),
                        .alignment_log2 = try decoder.parseOp(u16),
                        .section_index = try decoder.parseOptionalIndex(u32),
                        .visibility = try decoder.parseOp(G.Visibility),
                        .gc_index = try decoder.parseOptionalIndex(u32),
                        .unnamed_addr = try decoder.parseOp(G.UnnamedAddr),
                        .prologue_data_index = try decoder.parseOptionalIndex(u32),
                        .dll_storage_class = try decoder.parseOp(G.DllStorageClass),
                        .comdat = try decoder.skipOp(), // TODO
                        .prefix_index = try decoder.parseOptionalIndex(u32),
                        .personality_fn_index = try decoder.parseOptionalIndex(u32),
                        .preemption_specifier = try decoder.parseOp(G.PreemptionSpecifier),
                    };

                    const len = self.bc.module.function.len;
                    self.bc.module.function = try self.arena.allocator().realloc(self.bc.module.function, len + 1);
                    self.bc.module.function[len] = f;
                },
                .MODULE_CODE_VERSION => {
                    self.bc.module.version = try decoder.parseOp(u2);
                },
                .MODULE_CODE_TRIPLE => {
                    self.bc.module.triple = try decoder.parseRemainingOpsAsSliceAlloc(u8, self.arena.allocator());
                },
                .MODULE_CODE_DATALAYOUT => {
                    self.bc.module.data_layout = try decoder.parseRemainingOpsAsSliceAlloc(u8, self.arena.allocator());
                },
                .MODULE_CODE_ASM,
                .MODULE_CODE_SECTIONNAME,
                .MODULE_CODE_DEPLIB,
                .MODULE_CODE_ALIAS,
                .MODULE_CODE_GCNAME,
                .MODULE_CODE_SOURCE_FILENAME,
                => std.log.err("TODO: parse module record {s}", .{@tagName(code)}),
                _ => std.log.err("unknown code {}", .{code}),
            }
            return null;
        }

        fn parseTypeRecord(self: *Self, decoder: anytype) !?ParseError {
            const code = try decoder.parseRecordCode(Bitcode.Module.Type.Code);
            std.log.info("    code {}", .{code});
            switch (code) {
                .TYPE_CODE_NUMENTRY => {
                    if (self.bc.module.type.entries.len != 0) {
                        return try self.parseError("duplicate NUMENTRY for type table", .{});
                    }
                    const count = try decoder.parseOp(usize);
                    self.bc.module.type.entries = try self.arena.allocator().alloc(Bitcode.Module.Type.Entry, count);
                },
                .TYPE_CODE_VOID => return try self.appendTypeDefinition(.void),
                .TYPE_CODE_FLOAT => return try self.appendTypeDefinition(.float),
                .TYPE_CODE_DOUBLE => return try self.appendTypeDefinition(.double),
                .TYPE_CODE_LABEL => return try self.appendTypeDefinition(.label),
                .TYPE_CODE_OPAQUE => return try self.appendTypeDefinitionNamed(.{ .@"opaque" = undefined }),
                .TYPE_CODE_INTEGER => {
                    const width = try decoder.parseOp(u16);
                    return try self.appendTypeDefinition(.{ .integer = width });
                },
                .TYPE_CODE_POINTER => {
                    const t = Bitcode.Module.Type.Entry{ .pointer = .{
                        .pointee_type_index = try decoder.parseOp(u32),
                        .address_space = try decoder.parseOp(u16),
                    } };
                    return try self.appendTypeDefinition(t);
                },
                .TYPE_CODE_HALF => return try self.appendTypeDefinition(.float),
                .TYPE_CODE_ARRAY => {
                    return try self.appendTypeDefinition(.{ .array = .{
                        .element_count = try decoder.parseOp(u64),
                        .element_type_index = try decoder.parseOp(u32),
                    } });
                },
                .TYPE_CODE_VECTOR => {
                    return try self.appendTypeDefinition(.{ .vector = .{
                        .element_count = try decoder.parseOp(u64),
                        .element_type_index = try decoder.parseOp(u32),
                    } });
                },
                .TYPE_CODE_X86_FP80 => return try self.appendTypeDefinition(.x86_fp80),
                .TYPE_CODE_FP128 => return try self.appendTypeDefinition(.fp128),
                .TYPE_CODE_PPC_FP128 => return try self.appendTypeDefinition(.ppc_fp128),
                .TYPE_CODE_METADATA => return try self.appendTypeDefinition(.metadata),
                .TYPE_CODE_X86_MMX => return try self.appendTypeDefinition(.x86_mmx),
                .TYPE_CODE_STRUCT_ANON => {
                    return try self.appendTypeDefinition(.{ .@"struct" = .{
                        .name = null,
                        .is_packed = try decoder.parseOp(bool),
                        .element_type_indexes = try decoder.parseRemainingOpsAsSliceAlloc(u32, self.arena.allocator()),
                    } });
                },
                .TYPE_CODE_STRUCT_NAME => {
                    self.pending_type_name = try decoder.parseRemainingOpsAsSliceAlloc(u8, self.arena.allocator());
                },
                .TYPE_CODE_STRUCT_NAMED => {
                    return try self.appendTypeDefinitionNamed(.{ .@"struct" = .{
                        .name = undefined,
                        .is_packed = try decoder.parseOp(bool),
                        .element_type_indexes = try decoder.parseRemainingOpsAsSliceAlloc(u32, self.arena.allocator()),
                    } });
                },
                .TYPE_CODE_FUNCTION => {
                    return try self.appendTypeDefinition(.{ .function = .{
                        .is_vararg = try decoder.parseOp(bool),
                        .return_type_index = try decoder.parseOp(u32),
                        .param_type_indexes = try decoder.parseRemainingOpsAsSliceAlloc(u32, self.arena.allocator()),
                    } });
                },
                .TYPE_CODE_BFLOAT => return try self.appendTypeDefinition(.bfloat),
                .TYPE_CODE_X86_AMX => return try self.appendTypeDefinition(.x86_amx),
                _ => std.log.err("unknown code {}", .{code}),
            }
            return null;
        }

        fn appendTypeDefinition(self: *Self, entry: Bitcode.Module.Type.Entry) !?ParseError {
            if (self.type_entry_count == self.bc.module.type.entries.len) {
                return try self.parseError("found too many type entries according to NUMENTRY", .{});
            }
            self.bc.module.type.entries[self.type_entry_count] = entry;
            self.type_entry_count += 1;
            return null;
        }

        fn appendTypeDefinitionNamed(self: *Self, entry: Bitcode.Module.Type.Entry) !?ParseError {
            const name = self.pending_type_name orelse
                return try self.parseError("no pending type name for named {s}", .{@tagName(entry)});
            defer self.pending_type_name = null;

            switch (entry) {
                .@"opaque" => {
                    return try self.appendTypeDefinition(.{ .@"opaque" = .{
                        .name = name,
                    } });
                },
                .@"struct" => |s| {
                    return try self.appendTypeDefinition(.{ .@"struct" = .{
                        .name = name,
                        .is_packed = s.is_packed,
                        .element_type_indexes = s.element_type_indexes,
                    } });
                },
                else => unreachable,
            }
        }

        fn readAbbrevId(self: *Self) !bitstream.AbbreviationId {
            return self.bitstream_reader.readAbbreviationId(self.abbrev_id_width);
        }

        /// Parse bits during DEFINE_ABBREV, storing the abbreviation in
        /// the given block. An abbrev's ID is based on its index when appended.
        fn parseDefineAbbrev(self: *Self, block_id: Bitcode.BlockId) !?ParseError {
            const op_count = try self.bitstream_reader.readVbr(u32, 5);
            if (op_count == 0) {
                return try self.parseError("empty op list in abbrev", .{});
            }

            const id_encoding = try self.bitstream_reader.readAbbrevDefOp();
            switch (id_encoding) {
                .literal => {},
                .encoded => |enc| switch (enc) {
                    .fixed, .vbr => {},
                    .char6, .array, .blob => return try self.parseError(
                        "abbrev record code must be encoded as literal, fixed, or vbr, but got {s}",
                        .{@tagName(enc)},
                    ),
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
                while (i < arg_count) : (i += 1) {
                    const op = try self.bitstream_reader.readAbbrevDefOp();
                    std.log.info("  ABBREV OP: {any}", .{op});
                    if (op == .encoded and op.encoded == .array) {
                        if (i + 2 != arg_count) {
                            return try self.parseError("abbrev def array must be second to last op", .{});
                        }
                    }
                    ops[i] = op;
                }

                abbrev.op_encoding = ops;
            }

            const abbrevs = &self.getBlockInfo(block_id).abbrevs;
            try abbrevs.append(self.arena.allocator(), abbrev);
            return null;
        }

        /// Implements RecordDecoder that handles UNABBREV_RECORDs.
        const UnabbrevDecoder = struct {
            p: *Self,
            len: u32 = undefined,
            index: u32 = 0,

            fn readRecordCode(self: *UnabbrevDecoder) !u64 {
                const code = try self.p.bitstream_reader.readVbr(u64, 6);
                self.len = try self.p.bitstream_reader.readVbr(u32, 6);
                return code;
            }

            fn readOp(self: *UnabbrevDecoder) !?u64 {
                if (self.index == self.len) {
                    return null;
                }
                self.index += 1;
                return try self.p.bitstream_reader.readVbr(u64, 6);
            }
        };

        fn unabbrevDecoder(self: *Self) RecordDecoder(UnabbrevDecoder) {
            const impl = UnabbrevDecoder{ .p = self };
            return RecordDecoder(UnabbrevDecoder){ .impl = impl };
        }

        /// Implements RecordDecoder for abbreviated records.
        const AbbrevDecoder = struct {
            p: *Self,
            abbrev: Abbrev,
            // When array encoding is found, this is set to ops.len.
            index: u32 = 0,
            // When array encoding is found, this is set to parsed array len.
            remaining_array_elems: u32 = 0,

            fn readRecordCode(self: *AbbrevDecoder) !u64 {
                return switch (self.abbrev.id_encoding) {
                    .literal => |lit| lit,
                    .encoded => |enc| switch (enc) {
                        .fixed => |width| try self.p.bitstream_reader.readInt(u64, width),
                        .vbr => |width| try self.p.bitstream_reader.readVbr(u64, width),
                        .char6, .array, .blob => unreachable, // rejected during DEFINE_ABBREV
                    },
                };
            }

            fn readOp(self: *AbbrevDecoder) !?u64 {
                if (self.remaining_array_elems > 0) {
                    const encoding = self.abbrev.op_encoding[self.abbrev.op_encoding.len - 1];
                    self.remaining_array_elems -= 1;
                    return switch (encoding) {
                        .literal => |lit| lit,
                        .encoded => |enc| switch (enc) {
                            .fixed => |width| try self.p.bitstream_reader.readInt(u64, width),
                            .vbr => |width| try self.p.bitstream_reader.readVbr(u64, width),
                            .char6 => try self.p.bitstream_reader.readChar6(),
                            .array, .blob => unreachable, // rejected during DEFINE_ABBREV
                        },
                    };
                } else if (self.index == self.abbrev.op_encoding.len) {
                    return null;
                } else {
                    const encoding = self.abbrev.op_encoding[self.index];
                    self.index += 1;
                    return switch (encoding) {
                        .literal => |lit| lit,
                        .encoded => |enc| switch (enc) {
                            .fixed => |width| try self.p.bitstream_reader.readInt(u64, width),
                            .vbr => |width| try self.p.bitstream_reader.readVbr(u64, width),
                            .char6 => try self.p.bitstream_reader.readChar6(),
                            .array => {
                                self.remaining_array_elems = try self.p.bitstream_reader.readVbr(u32, 6);
                                self.index += 1;
                                assert(self.index == self.abbrev.op_encoding.len);
                                return try self.readOp();
                            },
                            .blob => {
                                @panic("TODO: read encoded blob");
                            },
                        },
                    };
                }
            }
        };

        /// Returns null if the given abbrev ID has not been defined in this stream.
        /// Asserts block_id is a known ID.
        fn abbrevDecoder(
            self: *Self,
            block_id: Bitcode.BlockId,
            abbr_id: bitstream.AbbreviationId,
        ) ?RecordDecoder(AbbrevDecoder) {
            const abbrev = self.getAbbrev(block_id, abbr_id) orelse return null;
            const impl = AbbrevDecoder{ .p = self, .abbrev = abbrev };
            return RecordDecoder(AbbrevDecoder){ .impl = impl };
        }
    };
}

pub fn parser(gpa: Allocator, reader: anytype) Parser(@TypeOf(reader)) {
    return Parser(@TypeOf(reader)).init(gpa, reader);
}

pub const RecordDecodeError = error{ EndOfRecord, Overflow } || bitstream.ReadError;

/// `Impl` must implement the following:
///
/// ```
/// // Will be called exactly once, before any other method.
/// readRecordCode(self: *Impl) bitstream.ReadError!u64
/// // Must return null when there are no more values left
/// readOp(self: *Impl) bitstream.ReadError!?u64
/// ```
pub fn RecordDecoder(comptime Impl: type) type {
    return struct {
        /// Returns error.Overflow instead of panicking.
        fn intCast(comptime T: type, x: anytype) !T {
            return std.math.cast(T, x) orelse error.Overflow;
        }

        /// Returns error.Overflow instead of panicking.
        /// TODO: If this can fail with an error instead of panicking (@intToEnum),
        /// lots of enums don't have to be marked non-exhaustive (Linkage, Visibility, etc.).
        fn intToEnum(comptime E: type, i: anytype) !E {
            const Tag = @typeInfo(E).Enum.tag_type;
            const int = try intCast(Tag, i);
            return @intToEnum(E, int);
        }

        impl: Impl,
        header_parsed: bool = false,

        const Self = @This();
        const Err = RecordDecodeError;

        pub fn parseRecordCode(self: *Self, comptime EnumType: type) !EnumType {
            assert(!self.header_parsed);
            self.header_parsed = true;
            return intToEnum(EnumType, try self.impl.readRecordCode());
        }

        fn mustReadOp(self: *Self) !u64 {
            assert(self.header_parsed);
            return (try self.impl.readOp()) orelse error.EndOfRecord;
        }

        pub fn parseOp(self: *Self, comptime T: type) !T {
            assert(self.header_parsed);
            const info = @typeInfo(T);
            switch (info) {
                .Enum => {
                    const Tag = info.Enum.tag_type;
                    const int = try self.mustReadOp();
                    return intToEnum(T, try intCast(Tag, int));
                },
                .Int => {
                    return intCast(T, try self.mustReadOp());
                },
                .Bool => {
                    return (try self.mustReadOp() != 0);
                },
                .Void => {
                    _ = try self.mustReadOp();
                },
                else => {
                    @compileError("cannot parse op into a " ++ @typeName(T));
                },
            }
        }

        pub fn parseOptionalIndex(self: *Self, comptime T: type) !?T {
            assert(self.header_parsed);
            const x = try self.parseOp(T);
            return if (x == 0) null else x - 1;
        }

        pub fn parseRemainingOpsAsSliceAlloc(self: *Self, comptime T: type, allocator: Allocator) ![]T {
            assert(self.header_parsed);
            var list = ArrayList(T).init(allocator);
            while (try self.impl.readOp()) |val| {
                try list.append(try intCast(T, val));
            }
            return list.toOwnedSlice();
        }

        pub fn skipOp(self: *Self) !void {
            try self.parseOp(void);
        }

        pub fn finishOps(self: *Self) !void {
            assert(self.header_parsed);
            while (try self.impl.readOp() != null) {}
        }
    };
}

pub fn dump(gpa: Allocator, r: anytype) !void {
    var p = parser(gpa, r);
    defer p.deinit();
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
