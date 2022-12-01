const std = @import("std");
const bitstream = @import("bitstream.zig");
const llvm = @import("llvm_bitcode.zig");

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

    const f = try std.fs.cwd().openFile(file_name.?, .{});
    defer f.close();

    if (dump) {
        llvm.dump(gpa.allocator(), f.reader()) catch |err| {
            switch (err) {
                error.InvalidBitcode => try stderr.print("File contains invalid LLVM bitcode\n", .{}),
                else => return err,
            }
            std.os.exit(1);
        };
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
