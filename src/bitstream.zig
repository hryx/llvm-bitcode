//! Handles the abstract bitstream container encoding format used by LLVM bitcode.

const std = @import("std");
const io = std.io;
const assert = std.debug.assert;

pub const codes = @import("bitstream/codes.zig");
const wlk = @import("bitstream/walker.zig");
pub const walker = wlk.walker;
pub const Walker = wlk.Walker;
pub const WalkerOptions = wlk.WalkerOptions;
pub const WalkError = wlk.WalkError;

pub fn decodeChar6(c: u6) u8 {
    return "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789._"[c];
}

pub fn encodeChar6(c: u8) ?u6 {
    return switch (c) {
        'a'...'z' => c - 'a',
        'A'...'Z' => c - 'A' + 26,
        '0'...'9' => c - '0' + 52,
        '.' => 62,
        '_' => 63,
        else => null,
    };
}

pub const BitstreamError = error{
    /// Unexpectedly reached the end of the Reader stream when expecting more bits.
    EndOfStream,
    /// The VBR being decoded does not fit into the requested type.
    Overflow,
    /// The bitstream can be decoded, but contains nonsense values.
    /// For example, an invalid abbrev encoding rule.
    InvalidBitstream,
};

pub const BlockHeader = struct {
    id: codes.block.Id,
    /// All abbreviation IDs in this block will be a fixed-width integer with this many bits.
    abbrev_width: u5,
    /// Number of 32-bit words taken up by this block.
    word_count: u32,
};

/// A single operand from an abbreviation definition.
/// Note that the successful parsing of a single operand does not mean
/// that the definition is valid. See `Reader.readAbbrevOp()` for details.
pub const AbbrevOp = union(enum) {
    constant: u64,
    fixed: u6,
    vbr: u5,
    char6,
    blob,
    array,
};

pub fn Reader(comptime ReaderType: type) type {
    return struct {
        bit_reader: BitReaderType,
        /// How many bits have been read so far.
        pos: usize,

        const Self = @This();
        const BitReaderType = io.BitReader(.Little, ReaderType);
        // pub const Error = BitstreamError || BitReaderType.Error;
        pub const Error = BitstreamError;

        pub fn init(unberlying_reader: ReaderType) Self {
            return Self{
                .bit_reader = io.bitReader(.Little, unberlying_reader),
                .pos = 0,
            };
        }

        /// Read a variable bit-width integer (VBR), returning it as a T.
        /// `T` must be an unsigned integer type no larger than u64.
        /// `width` is the number of bits in each VBR chunk, which is application-specific.
        ///
        /// Individual VBR chunks can max 32 bits wide, hence width is a u5.
        pub fn readVbr(self: *Self, comptime T: type, width: u5) error{EndOfStream}!T {
            const t_info = @typeInfo(T).Int;
            assert(t_info.bits <= 64 and t_info.signedness == .unsigned);
            assert(width > 0);

            const Chunk = u64;
            const ShiftT = std.math.Log2Int(Chunk);
            const val_bits = @intCast(ShiftT, width - 1);

            var val: T = 0;
            var shift: Chunk = 0;
            var more = true;

            // TODO: return error.Overflow if doesn't fit,
            // or error.InvalidBitstream if high bit still set after u32 is full
            while (more) : (shift += val_bits) {
                const res = try self.bit_reader.readBitsNoEof(Chunk, val_bits);
                const shifted = @shlExact(res, @intCast(ShiftT, shift));
                val += @intCast(T, shifted);
                more = (try self.bit_reader.readBitsNoEof(u1, 1)) == 1;
                self.pos += width;
            }

            return val;
        }

        /// Read a fixed-width integer, returning it as a T.
        /// `T` must be an unsigned integer type no larger than u64.
        /// `width` is the number of bits in the value's encoding, which is application-specific.
        pub fn readInt(self: *Self, comptime T: type, width: u6) error{EndOfStream}!T {
            const t_info = @typeInfo(T).Int;
            assert(t_info.bits <= 64 and t_info.signedness == .unsigned);
            assert(width > 0);

            const val = try self.bit_reader.readBitsNoEof(T, width);
            self.pos += width;
            return val;
        }

        /// Read a fixed-width integer, returning it as a T.
        /// Width of the fixed field is inferred from T;
        /// to read an int with a runtime-known width, use `readInt`.
        pub fn readIntAuto(self: *Self, comptime T: type) error{EndOfStream}!T {
            const bits = @typeInfo(T).Int.bits;
            return self.readInt(T, bits);
        }

        /// Read a 6-bit encoded character, returning it as an ASCII character.
        pub fn readChar6(self: *Self) error{EndOfStream}!u8 {
            const val = try self.bit_reader.readBitsNoEof(u6, 6);
            self.pos += 6;
            return decodeChar6(val);
        }

        /// Read the "magic" header at the start of a bitstream.
        /// Asserts that this is only called at the start of a bitstream.
        /// Does not validate the contents of the magic bytes.
        pub fn readMagic(self: *Self) error{EndOfStream}![4]u8 {
            assert(self.pos == 0);
            var buf: [4]u8 = undefined;
            try self.bit_reader.reader().readNoEof(&buf);
            self.pos += 4 * 8;
            return buf;
        }

        /// Read an abbreviation with the given VBR width (bit size).
        /// Caller must keep track of the ID width for the given block scope,
        /// which is determined by the block's header.
        /// For the outermost scope, always use an abbeviation width of 2.
        pub fn readAbbrevId(self: *Self, width: u5) error{EndOfStream}!codes.abbrev.Id {
            const id = try self.readInt(u32, width);
            return @intToEnum(codes.abbrev.Id, id);
        }

        /// Reads a single abbreviation definition operand.
        /// Beware that this does not validate a sensical abbreviation,
        /// so alone it does not detect that arrays or blobs are correctly defined.
        ///
        /// When parsing an abbreviation definition, the caller must ensure that:
        ///
        /// - If this returns .array, there is exactly one more operand following it,
        ///   and the final operand is a fixed, vbr, or char6.
        /// - If this returns .blob, there are zero more operands.
        pub fn readAbbrevOp(self: *Self) Error!AbbrevOp {
            const is_literal = try self.readIntAuto(u1) == 1;
            if (is_literal) {
                const value = try self.readVbr(u64, 8);
                return AbbrevOp{ .constant = value };
            } else {
                const encoding = @intToEnum(codes.abbrev.OpEncoding, try self.readIntAuto(u3));
                switch (encoding) {
                    .fixed => return AbbrevOp{ .fixed = try self.readVbr(u6, 5) },
                    .vbr => return AbbrevOp{ .vbr = try self.readVbr(u5, 5) },
                    .char6 => return AbbrevOp.char6,
                    .blob => return AbbrevOp.blob,
                    .array => return AbbrevOp.array,
                    _ => return error.InvalidBitstream,
                }
            }
        }

        pub fn readSubBlockHeader(self: *Self) Error!BlockHeader {
            const block_id = try self.readVbr(u32, 8);
            const abbrev_width = try self.readVbr(u5, 4);
            try self.alignToWord();
            const block_words = try self.readIntAuto(u32);
            return BlockHeader{
                .id = @intToEnum(codes.block.Id, block_id),
                .abbrev_width = abbrev_width,
                .word_count = block_words,
            };
        }

        pub fn endBlock(self: *Self) error{EndOfStream}!void {
            try self.alignToWord();
        }

        /// If the cursor is at a position which is not a multiple of 32,
        /// moves it to the next 32-bit aligned position.
        /// The bitstream format pads information with zeroes up to 32-bit boundaries in some cases.
        pub fn alignToWord(self: *Self) error{EndOfStream}!void {
            const off = self.pos % 32;
            if (off != 0) {
                try self.skipBits(32 - off);
            }
        }

        pub fn skipBits(self: *Self, bits: usize) error{EndOfStream}!void {
            const skip_bytes = bits / 8;
            try self.bit_reader.reader().skipBytes(skip_bytes, .{});
            const diff = @intCast(u3, bits % 8);
            _ = try self.bit_reader.readBitsNoEof(u8, diff);
            self.pos += bits;
        }

        pub fn skipWords(self: *Self, count: u32) error{EndOfStream}!void {
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

test "vbr" {
    const data = [_]u8{ 0b0011_1110, 0b1000_0111, 0b1111_1100 };
    var fbs = io.fixedBufferStream(&data);

    var r = reader(fbs.reader());
    try testing.expectEqual(@as(u8, 30), try r.readVbr(u8, 4));
    try testing.expectEqual(@as(usize, 8), r.pos);

    fbs.reset();
    r = reader(fbs.reader());
    try testing.expectEqual(@as(u64, 17214), try r.readVbr(u64, 9));
    try testing.expectEqual(@as(usize, 18), r.pos);
}

test "vbr big number" {
    var buf: [64]u8 = undefined;
    var fbs = io.fixedBufferStream(&buf);
    var w = io.bitWriter(.Little, fbs.writer());
    try w.writeBits(@as(u24, 0xaaaaad), 24);
    try w.writeBits(@as(u1, 1), 1);
    try w.writeBits(@as(u24, 0xaaaaaa), 24);
    try w.writeBits(@as(u1, 1), 1);
    try w.writeBits(@as(u24, 0xaaaa), 24);
    try w.writeBits(@as(u1, 0), 1);
    try w.flushBits();
    fbs.reset();
    var r = reader(fbs.reader());
    try testing.expectEqual(@as(u64, 0xaaaaaaaaaaaaaaad), try r.readVbr(u64, 25));
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
