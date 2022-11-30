//! Handles the bitstream container encoding format used by LLVM bitcode.

const std = @import("std");
const io = std.io;

pub const AbbreviationId = enum(u32) {
    END_BLOCK,
    ENTER_SUBBLOCK,
    DEFINE_ABBREV,
    UNABBREV_RECORD,
    _,
};

/// Standard block IDs
pub const BlockId = enum(u3) {
    BLOCKINFO,
    _,

    pub const first_application_block_id = 8;
};

pub fn Reader(comptime ReaderType: type) type {
    return struct {
        bit_reader: BitReaderType,
        abbrev_id_width: u32,

        const Self = @This();
        const BitReaderType = io.BitReader(.Little, ReaderType);

        pub fn init(unberlying_reader: ReaderType) Self {
            return Self{
                .bit_reader = io.bitReader(.Little, unberlying_reader),
                .abbrev_id_width = 2,
            };
        }

        pub fn readVbr(self: *Self, comptime T: type, width: u32) !T {
            const Log2T = std.meta.Int(.unsigned, std.math.log2(@typeInfo(T).Int.bits));

            var val: T = 0;
            var i: Log2T = 0;
            var more = true;

            while (more) : (i += 1) {
                const res = try self.bit_reader.readBitsNoEof(T, width - 1);
                more = (try self.bit_reader.readBitsNoEof(u1, 1)) == 1;
                val += @shlExact(res, i);
            }

            return val;
        }

        pub fn readMagic(self: *Self) ![4]u8 {
            var buf: [4]u8 = undefined;
            _ = try self.bit_reader.reader().readAll(&buf);
            return buf;
        }

        pub fn readAbbreviationId(self: *Self) !AbbreviationId {
            const id = try self.bit_reader.readBitsNoEof(u32, self.abbrev_id_width);
            return @intToEnum(AbbreviationId, id);
        }

        pub fn readBlockId(self: *Self, comptime T: type) !T {
            return self.bit_reader.readBitsNoEof(T, 8);
        }
    };
}

pub fn reader(underlying_reader: anytype) Reader(@TypeOf(underlying_reader)) {
    return Reader(@TypeOf(underlying_reader)).init(underlying_reader);
}

test "magic" {
    const data = [_]u8{ 'B', 'C', 0xc0, 0xde, 0xaa, 0xaa };

    var fbs = io.fixedBufferStream(&data);
    var r = reader(fbs.reader());
    const magic = try r.readMagic();
    try std.testing.expectEqualSlices(u8, data[0..4], &magic);
}
