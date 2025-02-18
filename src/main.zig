const std = @import("std");
// This needs to be public for the test
// at the end of this file to work.
pub const lexer = @import("./lexer.zig");
pub const ast = @import("./ast.zig");
pub const parser = @import("./parser.zig");
pub const type_inference = @import("./type_inference.zig");

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

    // Create Parser
    var fileParser = try parser.Parser.init(allocator, fileContents);
    defer fileParser.deinit();

    // Parse an expression
    const fileAst = try fileParser.parse();
    defer fileAst.deinit(allocator);

    var algorithmJ = type_inference.AlgorithmJ.init(allocator);
    defer algorithmJ.deinit();

    // Print the AST
    fileAst.debugPrint();

    const t = algorithmJ.getType(fileAst);
    try stdout.print("{any}", .{t});

    try bw.flush();
}

// This test collects all the tests from imports
test {
    std.testing.refAllDecls(@This());
}
