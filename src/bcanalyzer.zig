const std = @import("std");
const bitstream = @import("bitstream.zig");
const bitcode = @import("bitcode.zig");
const Bitcode = bitcode.Bitcode;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    var arena = std.heap.ArenaAllocator.init(gpa.allocator());
    defer arena.deinit();

    var stderr = std.io.getStdErr().writer();

    const args = try std.process.argsAlloc(arena.allocator());

    var file_name: ?[]const u8 = null;
    var dump = false;

    for (args[1..]) |arg| {
        if (std.mem.eql(u8, arg, "--dump")) {
            dump = true;
        } else if (std.mem.eql(u8, arg, "--stats")) {
            stderr.print("--stats option not implemented\n", .{}) catch unreachable;
            std.os.exit(1);
        } else {
            if (file_name != null) {
                stderr.print("Only one file name may be specified\n\n", .{}) catch unreachable;
                printUsage();
                std.os.exit(1);
            }
            file_name = arg;
        }
    }
    if (file_name == null) {
        stderr.print("No file name specified\n\n", .{}) catch unreachable;
        printUsage();
        std.os.exit(1);
    }

    const src = try std.fs.cwd().readFileAllocOptions(arena.allocator(), file_name.?, 20_000_000, 2_000_000, 4, null);

    const res = try bitcode.parser.parse(gpa.allocator(), src);
    defer res.deinit();
    switch (res.value) {
        .success => |bc| {
            if (dump) dumpBitcode(std.io.getStdOut().writer(), bc);
        },
        .failure => |err| {
            err.render(stderr) catch unreachable;
            stderr.print("\nFile contains invalid LLVM bitcode\n", .{}) catch unreachable;
            std.os.exit(1);
        },
    }
}

fn printUsage() void {
    var w = std.io.getStdErr().writer();
    w.print(
        \\usage: bcanalyzer [OPTIONS] FILE
        \\
        \\options:
        \\
        \\    --dump    Dump bitstream container structure in human-readable format
        \\    --stats   Print statistics about bitstream container
        \\
    , .{}) catch unreachable;
}

fn dumpBitcode(w: anytype, bc: Bitcode) void {
    std.json.stringify(bc, .{}, w) catch unreachable;
}
