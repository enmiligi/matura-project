const std = @import("std");
// This needs to be public for the test
// at the end of this file to work.
pub const lexer = @import("./lexer.zig");
pub const ast = @import("./ast.zig");
pub const parser = @import("./parser.zig");
pub const type_inference = @import("./type_inference.zig");
pub const interpreter = @import("./interpreter.zig");
pub const value = @import("./value.zig");
pub const errors = @import("./errors.zig");

const MainError = error{
    WrongUsage,
};

pub fn main() !u8 {
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

    var errs: errors.Errors = .{
        .stderr = stderr.any(),
        .source = fileContents,
        .fileName = consoleArgs[1],
        .allocator = allocator,
        .typeVarMap = .init(allocator),
    };
    defer errs.deinit();

    // Create Parser
    var fileParser = try parser.Parser.init(allocator, fileContents, &errs);
    defer fileParser.deinit();

    // Parse an expression
    const fileAst = fileParser.parseToEnd() catch |err| switch (err) {
        error.InvalidChar, error.UnexpectedToken, error.InvalidPrefix => {
            try errbw.flush();
            return 1;
        },
        else => {
            return err;
        },
    };
    defer fileAst.deinit(allocator);
    try fileAst.print(stdout.any());

    try stdout.print("\n", .{});

    var algorithmJ = type_inference.AlgorithmJ.init(allocator, &errs);

    const t = algorithmJ.getType(fileAst) catch |err| switch (err) {
        error.UnknownIdentifier, error.CouldNotUnify => {
            try errbw.flush();
            return 1;
        },
        else => {
            return err;
        },
    };
    defer t.deinit(allocator);

    var interpreter_ = try interpreter.Interpreter.init(allocator);
    defer interpreter_.deinit();
    const result = try interpreter_.eval(fileAst);
    try value.printValue(result, stdout.any());

    try stdout.print(": ", .{});

    var currentTypeVar: usize = 0;
    var typeVarMap = std.AutoHashMap(usize, usize).init(allocator);

    try type_inference.printType(t, stdout.any(), &currentTypeVar, &typeVarMap);
    try stdout.print("\n", .{});

    try bw.flush();

    return 0;
}

// This test collects all the tests from imports
test {
    std.testing.refAllDecls(@This());
}
