//! Handles reading and writing LLVM bitcode files.

const std = @import("std");
const Allocator = std.mem.Allocator;
const ArenaAllocator = std.heap.ArenaAllocator;

const bitstream = @import("bitstream.zig");

pub const magic = [4]u8{ 'B', 'C', 0xc0, 0xde };

pub const BlockId = enum(u16) {
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
    identification: Idendification,
    module: Module,
    symtab: Symtab,
    strtab: Strtab,

    pub const Idendification = struct {
        identification: []const u8,
        epoch: u0, // TODO
    };

    pub const Module = struct {
        version: u0, // TODO
        type: Type,
        param_attr_group: ParamAttrGroup,
        param_attr: ParamAttr,
        triple: []const u8,
        data_layout: []const u8,
        source_filename: []const u8,
        global_var: []const u8, // TODO ?
        function: []const u8, // TODO ?
        vst_offset: void, // TODO ?
        constants: Constants,

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
    success: *Bitcode,
    failure: Error,

    pub const Error = union(enum) {
        @"bad magic",
        @"expected ENTER_SUBBLOCK",
    };
};

pub fn Parser(comptime ReaderType: type) type {
    return struct {
        arena: ArenaAllocator,
        bitstream_reader: bitstream.Reader(ReaderType),

        const Self = @This();

        pub fn init(gpa: Allocator, reader: ReaderType) Self {
            return Self{
                .arena = ArenaAllocator.init(gpa),
                .bitstream_reader = bitstream.reader(reader),
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

            const abbrev_id = try self.bitstream_reader.readAbbreviationId();
            if (abbrev_id != .ENTER_SUBBLOCK) {
                return ParseResult{ .failure = .@"expected ENTER_SUBBLOCK" };
            }

            const block_id = try self.readBlockId();
            _ = block_id;

            return ParseResult{ .success = undefined };
        }

        fn readBlockId(self: *Self) !BlockId {
            return @intToEnum(BlockId, try self.bitstream_reader.readBlockId(std.meta.Tag(BlockId)));
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
