//! Handles reading and writing LLVM bitcode files.

const std = @import("std");
const assert = std.debug.assert;
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;
const ArenaAllocator = std.heap.ArenaAllocator;

const bitstream = @import("bitstream.zig");
const codes = bitstream.codes;

pub const Bitcode = @import("Bitcode.zig");
pub const parser = @import("bitcode/parser.zig");

test {
    _ = bitstream;
    _ = Bitcode;
}
