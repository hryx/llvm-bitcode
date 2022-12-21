const std = @import("std");
const io = std.io;
const math = std.math;
const assert = std.debug.assert;

const bitstream = @import("../bitstream.zig");

pub fn Writer(comptime WriterType: type) type {
    return struct {
        bit_writer: BitWriterType,
        pos: usize,

        const Self = @This();
        const BitWriterType = io.BitWriter(.Little, WriterType);

        pub fn init(underling_writer: WriterType) Self {
            return Self{
                .bit_writer = io.bitWriter(.Little, underling_writer),
                .pos = 0,
            };
        }

        /// Asserts `value` is an unsigned int no larger than u64.
        /// `width` includes the continuation bit of each chunk,
        /// so must be at least 2.
        pub fn writeVbr(self: *Self, value: anytype, width: u5) !void {
            const T = @TypeOf(value);
            const t_info = @typeInfo(T).Int;
            assert(t_info.bits <= 64 and t_info.signedness == .unsigned);
            assert(width > 1);

            const chunk_size = width - 1;
            var val = value;
            var more = true;

            while (more) {
                try self.bit_writer.writeBits(val, chunk_size);
                val = math.shr(T, val, chunk_size);
                more = @clz(val) < t_info.bits;
                try self.bit_writer.writeBits(@boolToInt(more), 1);
                self.pos += width;
            }
        }

        /// Asserts `value` is an unsigned int no larger than u64.
        /// If `width` is larger than bit size of `value`,
        /// the remainder is padded with zeroes.
        pub fn writeInt(self: *Self, value: anytype, width: u6) !void {
            const T = @TypeOf(value);
            const t_info = @typeInfo(T).Int;
            assert(t_info.bits <= 64 and t_info.signedness == .unsigned);
            assert(width >= t_info.bits);
            const zeroes_width = width - t_info.bits;
            try self.bit_writer.writeBits(value, width);
            try self.bit_writer.writeBits(@as(u64, 0), zeroes_width);
        }

        /// Asserts `value` is an unsigned int no larger than u64.
        /// Writes a fixed-width int whose width is determined by the
        /// number of bits in type of `value`.
        pub fn writeIntAuto(self: *Self, value: anytype) !void {
            const T = @TypeOf(value);
            const t_info = @typeInfo(T).Int;
            try self.writeInt(value, t_info.bits);
        }

        /// Asserts that `char` is a valid char6 character.
        /// When unsure, use `encodeChar6` or `isValidChar6String`.
        pub fn writeChar6(self: *Self, char: u8) !void {
            const c = bitstream.encodeChar6(char).?;
            try self.bit_writer.writeBits(c, 6);
        }

        pub fn writeMagic(self: *Self, bytes: *const [4]u8) !void {
            try self.bit_writer.writer().writeAll(bytes);
        }
    };
}

pub fn writer(underlying_writer: anytype) Writer(@TypeOf(underlying_writer)) {
    return Writer(@TypeOf(underlying_writer)).init(underlying_writer);
}

const testing = std.testing;

test "writer: magic" {
    var buf: [12]u8 = undefined;
    var fbs = io.fixedBufferStream(&buf);
    var w = writer(fbs.writer());
    try w.writeMagic("ABCD");
    try w.writeMagic(&.{ 1, 2, 3, 4 });
    const src: []const u8 = "hello";
    try w.writeMagic(src[0..4]);
}

test "writer: vbr" {
    var buf: [4]u8 = undefined;
    var fbs = io.fixedBufferStream(&buf);
    var w = writer(fbs.writer());

    try w.writeVbr(@as(u10, 0), 10); // => 0000000000
    try w.writeVbr(@as(u64, 200), 4); // => 000, 001, 011
    try w.bit_writer.flushBits();

    try testing.expectEqual(@as(usize, 22), w.pos);
    try testing.expectEqualSlices(u8, &.{
        0b0000_0000,
        0b0110_0000,
        0b0000_1110,
    }, buf[0..3]);
}

test "writer: fixed int" {
    var buf: [4]u8 = undefined;
    var fbs = io.fixedBufferStream(&buf);
    var w = writer(fbs.writer());

    try w.writeIntAuto(@as(u14, 1234));
    try w.writeIntAuto(@as(u11, 999));
    try w.bit_writer.flushBits();

    try testing.expectEqualSlices(u8, &.{
        0b1101_0010,
        0b1100_0100,
        0b1111_1001,
        0b0000_0000,
    }, buf[0..4]);
}
