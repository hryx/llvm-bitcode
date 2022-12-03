//! Handles the abstract bitstream container encoding format used by LLVM bitcode.

const std = @import("std");
const io = std.io;
const assert = std.debug.assert;

// TODO: rename to AbbrevId for consitency
pub const AbbreviationId = enum(u32) {
    END_BLOCK,
    ENTER_SUBBLOCK,
    DEFINE_ABBREV,
    UNABBREV_RECORD,
    _,

    pub const first_application_abbrev_id = 4;
};

/// Standard block IDs
pub const BlockId = enum(u32) {
    BLOCKINFO,
    _,

    /// All application-specific headers must be this value or higher.
    pub const first_application_block_id = 8;
};

pub const BlockInfoCode = enum(u32) {
    /// Sets which block ID following records apply to,
    /// instead of the current block (which is a BLOCKINFO).
    /// Must be the first record in a BLOCKINFO.
    BLOCKINFO_CODE_SETBID = 1,
    BLOCKINFO_CODE_BLOCKNAME,
    BLOCKINFO_CODE_SETRECORDNAME,
    /// BLOCKINFO does not allow for application-specific codes.
    _,
};

pub const BlockHeader = struct {
    id: BlockId,
    new_abbr_id_width: u32,
    word_count: u32,
};

pub const AbbrevDefOp = union(enum) {
    literal: u64,
    encoded: Encoding,

    /// For details of the meaning of these encodings, see:
    /// https://www.llvm.org/docs/BitCodeFormat.html#define-abbrev-encoding
    pub const Encoding = union(Tag) {
        pub const Tag = enum(u3) {
            fixed = 1,
            vbr,
            array,
            char6,
            blob,
            _,
        };

        fixed: u16,
        vbr: u16,
        array,
        char6,
        blob,
    };
};

pub fn decodeChar6(c: u6) u8 {
    return "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789._"[c];
}

// TODO: use this
pub const ReadError = error{ EndOfStream, InvalidBitstream };

// TODO: Should read* functions implicitly return u64 instead of taking T: type?
// The spec does not mention a general max size for fixed ints or VBRs.
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
        pub fn readVbr(self: *Self, comptime T: type, width: u16) !T {
            const ShiftT = std.math.Log2Int(T);
            const val_bits = @intCast(ShiftT, width - 1);

            var val: T = 0;
            var shift: ShiftT = 0;
            var more = true;

            while (more) : (shift += val_bits) {
                const res = try self.bit_reader.readBitsNoEof(T, val_bits);
                more = (try self.bit_reader.readBitsNoEof(u1, 1)) == 1;
                val += @shlExact(res, shift);
                self.pos += width;
            }

            return val;
        }

        /// Read a fixed-width integer, returning it as a T.
        pub fn readInt(self: *Self, comptime T: type, width: u16) !T {
            const val = try self.bit_reader.readBitsNoEof(T, width);
            self.pos += width;
            return val;
        }

        /// Read a fixed-width integer, returning it as a T.
        /// T must be no larger than 32 bits.
        /// Width of the fixed field is inferred from T;
        /// to read an int with a runtime-known width, use `readInt`.
        pub fn readIntAuto(self: *Self, comptime T: type) !T {
            const bits = @typeInfo(T).Int.bits;
            assert(bits <= 32);
            return self.readInt(T, bits);
        }

        /// Read a 6-bit encoded character, returning it as an ASCII character.
        pub fn readChar6(self: *Self) !u8 {
            const val = try self.bit_reader.readBitsNoEof(u6, 6);
            self.pos += 6;
            return decodeChar6(val);
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

        pub fn readAbbrevDefOp(self: *Self) !AbbrevDefOp {
            const is_literal = try self.readIntAuto(u1) == 1;
            if (is_literal) {
                const value = try self.readVbr(u32, 8);
                return AbbrevDefOp{ .literal = value };
            } else {
                const encoding = @intToEnum(AbbrevDefOp.Encoding.Tag, try self.readIntAuto(u3));
                switch (encoding) {
                    .fixed => return AbbrevDefOp{ .encoded = .{ .fixed = try self.readVbr(u16, 5) } },
                    .vbr => return AbbrevDefOp{ .encoded = .{ .vbr = try self.readVbr(u16, 5) } },
                    inline .array, .char6, .blob => |enc| return AbbrevDefOp{ .encoded = enc },
                    _ => return error.InvalidBitstream,
                }
            }
        }

        pub fn readSubBlockHeader(self: *Self) !BlockHeader {
            const block_id = try self.readVbr(u32, 8);
            const new_abbr_id_width = try self.readVbr(u32, 4);
            try self.alignToWord();
            const block_words = try self.readIntAuto(u32);
            return BlockHeader{
                .id = @intToEnum(BlockId, block_id),
                .new_abbr_id_width = new_abbr_id_width,
                .word_count = block_words,
            };
        }

        pub fn endBlock(self: *Self) !void {
            try self.alignToWord();
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

test "vbr" {
    const data = [_]u8{ 0b0011_1110, 0b1000_0111, 0b1111_1100 };
    var fbs = io.fixedBufferStream(&data);

    var r = reader(fbs.reader());
    try testing.expectEqual(@as(u8, 30), try r.readVbr(u8, 4));
    try testing.expectEqual(@as(usize, 8), r.pos);

    fbs.reset();
    r = reader(fbs.reader());
    try testing.expectEqual(@as(u99, 17214), try r.readVbr(u99, 9));
    try testing.expectEqual(@as(usize, 18), r.pos);
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
