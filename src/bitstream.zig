//! Handles the abstract bitstream container encoding format used by LLVM bitcode.

const std = @import("std");

pub const codes = @import("bitstream/codes.zig");

pub const reader = @import("bitstream/reader.zig").reader;
pub const Reader = @import("bitstream/reader.zig").Reader;
pub const writer = @import("bitstream/writer.zig").writer;
pub const Writer = @import("bitstream/writer.zig").Writer;
pub const walker = @import("bitstream/walker.zig").walker;
pub const Walker = @import("bitstream/walker.zig").Walker;
pub const WalkerOptions = @import("bitstream/walker.zig").WalkerOptions;
pub const WalkError = @import("bitstream/walker.zig").WalkError;

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

pub fn isValidChar6String(str: []const u8) bool {
    for (str) |c| if (encodeChar6(c) == null) return false;
    return true;
}

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

test {
    _ = @import("bitstream/reader.zig");
    _ = @import("bitstream/writer.zig");
    _ = @import("bitstream/walker.zig");
}
