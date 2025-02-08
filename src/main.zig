const std = @import("std");

const MainError = error{
    WrongUsage,
};

pub fn main() !void {
    // Initialize allocator for memory management
    var gpa = std.heap.GeneralPurposeAllocator(.{}).init;
    const allocator = gpa.allocator();

    // Initialize stdin and stdout
    const stdout_file = std.io.getStdOut().writer();
    var bw = std.io.bufferedWriter(stdout_file);
    const stdout = bw.writer();

    const stderr_file = std.io.getStdErr().writer();
    var errbw = std.io.bufferedWriter(stderr_file);
    const stderr = errbw.writer();

    // Read commandline options
    const consoleArgs = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, consoleArgs);

    // Ensure correct usage
    if (consoleArgs.len != 2) {
        try stderr.print("Wrong number of arguments supplied.\n", .{});
        try stderr.print("Usage: matura-project file\n", .{});
        try errbw.flush();
        return MainError.WrongUsage;
    }

    // Initialize file
    const cwd = std.fs.cwd();
    const file = cwd.openFile(consoleArgs[1], .{ .mode = .read_only }) catch |err| {
        switch (err) {
            error.FileNotFound => {
                try stderr.print("File {s} does not exist.\n", .{consoleArgs[1]});
                try errbw.flush();
            },
            else => {},
        }
        return err;
    };
    defer file.close();

    // Read file
    const fileContents = try file.readToEndAlloc(allocator, std.math.maxInt(usize));
    try stdout.print("{s}", .{fileContents});

    try bw.flush();
}
