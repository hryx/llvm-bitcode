//! Handles the bitstream container encoding format used by LLVM bitcode.

const std = @import("std");
const io = std.io;
const assert = std.debug.assert;

pub const AbbreviationId = enum(u32) {
    END_BLOCK,
    ENTER_SUBBLOCK,
    DEFINE_ABBREV,
    UNABBREV_RECORD,
    _,
};

/// Standard block IDs
pub const BlockId = enum(u32) {
    BLOCKINFO,
    _,

    /// All application-specific headers must be this value or higher.
    pub const first_application_block_id = 8;
};

pub const BlockHeader = struct {
    id: BlockId,
    new_abbr_id_width: u32,
    word_count: u32,
};

pub fn Reader(comptime ReaderType: type) type {
    return struct {
        bit_reader: BitReaderType,
        /// How many bits have been read so far.
        pos: usize,

        const Self = @This();
        const BitReaderType = io.BitReader(.Little, ReaderType);

        pub fn init(unberlying_reader: ReaderType) Self {
            return Self{
                .bit_reader = io.bitReader(.Little, unberlying_reader),
                .pos = 0,
            };
        }

        /// Read a variable bit-width integer (VBR), returning it as a T.
        /// `T` must be an unsigned integer type.
        /// `width` is the number of bits in the value's encoding, which is application-specific.
        pub fn readVbr(self: *Self, comptime T: type, width: u32) !T {
            const ShiftT = std.math.Log2Int(T);

            var val: T = 0;
            var i: ShiftT = 0;
            var more = true;

            while (more) : (i += 1) {
                const res = try self.bit_reader.readBitsNoEof(T, width - 1);
                more = (try self.bit_reader.readBitsNoEof(u1, 1)) == 1;
                val += @shlExact(res, i);
                self.pos += width;
            }

            return val;
        }

        /// Read a fixed-sized integer. T must be no larger than 32 bits.
        pub fn readInt(self: *Self, comptime T: type) !T {
            const bits = @typeInfo(T).Int.bits;
            assert(bits <= 32);
            const val = try self.bit_reader.readBitsNoEof(T, bits);
            self.pos += bits;
            return val;
        }

        /// Read the "magic" header at the start of a bitstream.
        /// This must be the first read function called on a bitstream.
        pub fn readMagic(self: *Self) ![4]u8 {
            var buf: [4]u8 = undefined;
            self.pos += try self.bit_reader.reader().readAll(&buf) * 8;
            return buf;
        }

        /// Read an abbreviation with the given VBR width (bit size).
        /// Caller must keep track of the ID width for the given block scope,
        /// which is determined by the block's header.
        /// For the outermost scope, always use an abbeviation width of 2.
        pub fn readAbbreviationId(self: *Self, width: u32) !AbbreviationId {
            const id = try self.bit_reader.readBitsNoEof(u32, width);
            self.pos += width;
            return @intToEnum(AbbreviationId, id);
        }

        pub fn readSubBlockHeader(self: *Self) !BlockHeader {
            const block_id = try self.readVbr(u32, 8);
            const new_abbr_id_width = try self.readVbr(u32, 4);
            try self.alignToWord();
            const block_words = try self.readInt(u32);
            return BlockHeader{
                .id = @intToEnum(BlockId, block_id),
                .new_abbr_id_width = new_abbr_id_width,
                .word_count = block_words,
            };
        }

        /// If the cursor is at a position which is not a multiple of 32,
        /// moves it to the next 32-bit aligned position.
        /// The bitstream format pads information with zeroes up to 32-bit boundaries in some cases.
        pub fn alignToWord(self: *Self) !void {
            const off = self.pos % 32;
            if (off != 0) {
                try self.skipBits(32 - off);
            }
        }

        pub fn skipBits(self: *Self, bits: usize) !void {
            const skip_bytes = bits / 8;
            try self.bit_reader.reader().skipBytes(skip_bytes, .{});
            const diff = @intCast(u3, bits % 8);
            _ = try self.bit_reader.readBitsNoEof(u8, diff);
            self.pos += bits;
        }

        pub fn skipWords(self: *Self, count: u32) !void {
            try self.skipBits(@as(usize, count) * 32);
        }
    };
}

pub fn reader(underlying_reader: anytype) Reader(@TypeOf(underlying_reader)) {
    return Reader(@TypeOf(underlying_reader)).init(underlying_reader);
}

const testing = std.testing;

test "magic" {
    const data = [_]u8{ 'B', 'C', 0xc0, 0xde, 0xaa, 0xaa };

    var fbs = io.fixedBufferStream(&data);
    var r = reader(fbs.reader());
    const magic = try r.readMagic();
    try testing.expectEqualSlices(u8, data[0..4], &magic);
}

test "skip bits" {
    const data = "good day my dude, have one on the house";

    var fbs = io.fixedBufferStream(data);
    var r = reader(fbs.reader());

    try testing.expectEqual(@as(usize, 0), r.pos);
    try r.alignToWord();
    try testing.expectEqual(@as(usize, 0), r.pos);

    try r.skipBits(1);
    try testing.expectEqual(@as(usize, 1), r.pos);
    try r.skipBits(6);
    try testing.expectEqual(@as(usize, 7), r.pos);
    try r.skipBits(0);
    try testing.expectEqual(@as(usize, 7), r.pos);
    try r.alignToWord();
    try testing.expectEqual(@as(usize, 32), r.pos);
    try r.alignToWord();

    try testing.expectEqual(@as(usize, 32), r.pos);
    try r.skipBits(9);
    try testing.expectEqual(@as(usize, 41), r.pos);
    try r.skipWords(2);
    try testing.expectEqual(@as(usize, 105), r.pos);
    try r.alignToWord();
    try testing.expectEqual(@as(usize, 128), r.pos);
}
