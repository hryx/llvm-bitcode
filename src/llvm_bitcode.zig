//! Handles reading and writing LLVM bitcode files.

const std = @import("std");
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;
const ArenaAllocator = std.heap.ArenaAllocator;

const bitstream = @import("bitstream.zig");

pub const magic = [4]u8{ 'B', 'C', 0xc0, 0xde };

pub const BlockId = enum(std.meta.Tag(bitstream.BlockId)) {
    MODULE_BLOCK_ID = bitstream.BlockId.first_application_block_id,
    PARAMATTR_BLOCK_ID,
    PARAMATTR_GROUP_BLOCK_ID,
    CONSTANTS_BLOCK_ID,
    FUNCTION_BLOCK_ID,
    IDENTIFICATION_BLOCK_ID,
    VALUE_SYMTAB_BLOCK_ID,
    METADATA_BLOCK_ID,
    METADATA_ATTACHMENT_ID,
    TYPE_BLOCK_ID_NEW,
    USELIST_BLOCK_ID,
    MODULE_STRTAB_BLOCK_ID,
    GLOBALVAL_SUMMARY_BLOCK_ID,
    OPERAND_BUNDLE_TAGS_BLOCK_ID,
    METADATA_KIND_BLOCK_ID,
    STRTAB_BLOCK_ID,
    FULL_LTO_GLOBALVAL_SUMMARY_BLOCK_ID,
    SYMTAB_BLOCK_ID,
    SYNC_SCOPE_NAMES_BLOCK_ID,
    _,
};

pub const Bitcode = struct {
    // TODO: Make non-optional, track when found in parse ctx
    identification: ?Idendification = null,
    module: ?Module = null,
    symtab: ?Symtab = null,
    strtab: ?Strtab = null,

    pub const Idendification = struct {
        identification: []const u8,
        epoch: u0, // TODO
    };

    pub const Module = struct {
        version: u2 = undefined,
        type: Type = undefined,
        param_attr_group: ParamAttrGroup = undefined,
        param_attr: ParamAttr = undefined,
        triple: []const u8 = undefined,
        data_layout: []const u8 = undefined,
        source_filename: []const u8 = undefined,
        global_var: []const u8 = undefined,
        function: []const u8 = undefined,
        vst_offset: void = {},
        constants: Constants = undefined,

        pub const Code = enum(u32) {
            MODULE_CODE_VERSION = 1,
            MODULE_CODE_TRIPLE,
            MODULE_CODE_DATALAYOUT,
            MODULE_CODE_ASM,
            MODULE_CODE_SECTIONNAME,
            MODULE_CODE_DEPLIB,
            MODULE_CODE_GLOBALVAR,
            MODULE_CODE_FUNCTION,
            MODULE_CODE_ALIAS,

            MODULE_CODE_GCNAME = 11,

            _,
        };

        pub const Type = struct {
            // TODO
        };

        pub const ParamAttrGroup = struct {
            // TODO
        };

        pub const ParamAttr = struct {
            // TODO
        };

        pub const Constants = struct {
            // TODO
        };

        pub const MetadataKind = struct {
            // TODO
        };

        pub const Metadata = struct {
            // TODO
        };
    };

    pub const Symtab = struct {
        // TODO
    };

    pub const Strtab = struct {
        // TODO
    };
};

pub const ParseResult = union(enum) {
    success: Bitcode,
    failure: Error,

    pub const Error = union(enum) {
        @"TODO: better error",
        @"bad magic",
        @"expected ENTER_SUBBLOCK",
        @"unknown block ID",
        @"duplicate module block",
        @"duplicate identification block",
    };
};

pub fn Parser(comptime ReaderType: type) type {
    return struct {
        arena: ArenaAllocator,
        bitstream_reader: bitstream.Reader(ReaderType),
        abbrev_id_width: u32,

        const Self = @This();

        pub fn init(gpa: Allocator, reader: ReaderType) Self {
            return Self{
                .arena = ArenaAllocator.init(gpa),
                .bitstream_reader = bitstream.reader(reader),
                .abbrev_id_width = 2,
            };
        }

        pub fn deinit(self: *Self) void {
            self.arena.deinit();
        }

        pub fn parse(self: *Self) !ParseResult {
            const file_magic = try self.bitstream_reader.readMagic();
            if (!std.mem.eql(u8, &file_magic, &magic)) {
                return ParseResult{ .failure = .@"bad magic" };
            }

            var bc = Bitcode{};

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
                    return ParseResult{ .failure = .@"expected ENTER_SUBBLOCK" };
                }

                if (try self.parseSubBlock(&bc)) |fail| {
                    return ParseResult{ .failure = fail };
                }
            }

            return ParseResult{ .success = bc };
        }

        fn parseSubBlock(self: *Self, bc: *Bitcode) !?ParseResult.Error {
            const header = try self.bitstream_reader.readSubBlockHeader();
            self.abbrev_id_width = header.new_abbr_id_width;
            std.log.info("got header block: {any}", .{header});
            switch (header.id) {
                .BLOCKINFO => {
                    @panic("TODO: BLOCKINFO");
                },
                _ => {},
            }

            const prev_abbr_width = self.abbrev_id_width;
            defer self.abbrev_id_width = prev_abbr_width;
            const id = @intToEnum(BlockId, @enumToInt(header.id));
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
                _ => return ParseResult.Error.@"unknown block ID",
            }
            return null;
        }

        fn parseModuleBlock(self: *Self, bc: *Bitcode) !?ParseResult.Error {
            if (bc.module != null) {
                return ParseResult.Error.@"duplicate module block";
            }
            bc.module = .{};
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
                                    return ParseResult.Error.@"TODO: better error";
                                }
                                const op = try self.bitstream_reader.readVbr(u32, 6);
                                if (op != 2) {
                                    return ParseResult.Error.@"TODO: better error";
                                }
                                bc.module.?.version = @intCast(u2, op);
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
                                return ParseResult.Error.@"TODO: better error";
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

        fn parseIdentificationBlock(self: *Self, bc: *Bitcode) !?ParseResult.Error {
            if (bc.identification != null) {
                return ParseResult.Error.@"duplicate identification block";
            }
            const id = try self.bitstream_reader.readAbbreviationId(self.abbrev_id_width);
            _ = id;
            // TODO: bc.identification = x;
            return null;
        }

        fn parseVbrSlice(self: *Self, comptime T: type, len: u32) ![]T {
            var list = ArrayList(T).init(self.arena);
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
            std.log.err("{s}", .{@tagName(err)});
            return error.InvalidBitcode;
        },
    }
}

test {
    _ = bitstream;
}
