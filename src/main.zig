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
pub const optimizer = @import("./optimizer.zig");

const MainError = error{
    WrongUsage,
};

pub fn main() !u8 {
    // Initialize allocator for memory management
    var gpa = std.heap.GeneralPurposeAllocator(.{ .stack_trace_frames = 6 }).init;
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

    var algorithmJ = type_inference.AlgorithmJ.init(allocator, &errs);
    defer algorithmJ.deinit();

    // Create Parser
    var fileParser = try parser.Parser.init(allocator, fileContents, &errs, &algorithmJ);
    defer fileParser.deinit();

    // // Parse an expression
    // // Set depth to max to ensure annotated type doesn't prevent generalization
    algorithmJ.depth = std.math.maxInt(usize);
    const statements = fileParser.file() catch |err| switch (err) {
        error.InvalidChar, error.UnexpectedToken, error.InvalidPrefix => {
            try errbw.flush();
            return 1;
        },
        else => {
            return err;
        },
    };
    // // Reset depth
    algorithmJ.depth = 0;
    defer ast.Statement.deinitStatements(statements, allocator);

    for (statements.items) |*statement| {
        algorithmJ.checkStatement(statement.*) catch |err| switch (err) {
            error.UnknownIdentifier,
            error.CouldNotUnify,
            error.InfiniteType,
            error.TooGeneral,
            error.NonExhaustiveMatch,
            => {
                try errbw.flush();
                return err;
            },
            else => {
                return err;
            },
        };
        try optimizer.optimizeStatement(statement, allocator);
    }

    if (algorithmJ.globalTypes.get("main") == null) {
        try errs.printError("No 'main' defined", .{});
        try errbw.flush();
        return 1;
    }

    for (statements.items) |statement| {
        try statement.print(stdout.any(), allocator);
        try stdout.print("\n", .{});
    }
    try bw.flush();

    var initialEnv: std.StringHashMap(value.Value) = .init(allocator);
    var interpreter_ = try interpreter.Interpreter.init(allocator, &initialEnv);
    defer interpreter_.deinit();
    for (statements.items) |statement| {
        try interpreter_.runStatement(statement);
    }
    const result = interpreter_.lookup("main").?;
    try value.printValue(result, stdout.any());

    try stdout.print(": ", .{});

    var currentTypeVar: usize = 0;
    var typeVarMap = std.AutoHashMap(usize, usize).init(allocator);
    defer typeVarMap.deinit();

    const mainTypeScheme = algorithmJ.globalTypes.get("main").?;
    var mainType: *type_inference.Type = undefined;
    switch (mainTypeScheme.*) {
        .type => |t| {
            t.rc += 1;
            mainType = t;
        },
        .forall => |*forall| {
            mainType = try algorithmJ.instantiate(forall);
        },
    }
    defer mainType.deinit(allocator);
    try type_inference.printType(
        mainType,
        stdout.any(),
        &currentTypeVar,
        &typeVarMap,
        true,
        allocator,
    );
    try stdout.print("\n", .{});

    try bw.flush();

    return 0;
}

// This test collects all the tests from imports
test {
    std.testing.refAllDecls(@This());
}
