const std = @import("std");
// This needs to be public for the test
// at the end of this file to work.
pub const lexer = @import("./lexer.zig");

const MainError = error{
    WrongUsage,
};

pub fn main() !void {
    // Initialize allocator for memory management
    var gpa = std.heap.GeneralPurposeAllocator(.{}).init;
    const allocator = gpa.allocator();
    defer _ = gpa.deinit();

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
    defer allocator.free(fileContents);

    // Create Lexer
    var fileLexer = lexer.Lexer{ .source = fileContents };

    // Print all generated Tokens
    var token: lexer.token.Token = undefined;
    while (!fileLexer.isAtEnd()) {
        token = try fileLexer.getToken();
        try stdout.print(
            "Token of type {any} with content \"{s}\" from char {d} to char {d}\n",
            .{ token.type, token.lexeme, token.start, token.end },
        );
        try bw.flush();
    }

    try bw.flush();
}

// This test collects all the tests from imports
test {
    std.testing.refAllDecls(@This());
}
