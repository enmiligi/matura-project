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
pub const desugaring = @import("desugaring.zig");
pub const runner = @import("./runner.zig");
pub const compiler = @import("compiler.zig");
pub const monomorphization = @import("monomorphization.zig");

const MainError = error{
    WrongUsage,
};

pub fn main() !u8 {
    // Initialize allocator for memory management
    var gpa = std.heap.GeneralPurposeAllocator(.{}).init;
    const allocator = gpa.allocator();
    defer _ = gpa.deinit();

    // Initialize stdin and stdout
    const stdout_file = std.io.getStdOut().writer().any();
    var bw = std.io.bufferedWriter(stdout_file);
    const stdout = bw.writer();

    const stderr_file = std.io.getStdErr().writer().any();
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

    // Create error handler
    var errs: errors.Errors = .{
        .stderr = stderr.any(),
        .source = undefined,
        .fileName = undefined,
        .allocator = allocator,
        .typeVarMap = .init(allocator),
    };
    defer errs.deinit();

    // Get standard input
    const stdin = std.io.getStdIn().reader();
    var stdinBW = std.io.bufferedReader(stdin);

    // Create runner
    var fileRunner = runner.Runner.init(allocator);
    defer fileRunner.deinit();

    // Create the algorithm to infer and check types
    var algorithmJ = type_inference.AlgorithmJ.init(allocator, &errs);
    defer algorithmJ.deinit();

    // Create Parser
    var fileParser = try parser.Parser.init(allocator, &errs, &algorithmJ);
    defer fileParser.deinit();

    // Create the initial environment and the interpreter
    var initialEnv: std.StringHashMap(value.Value) = .init(allocator);
    var interpreter_ = try interpreter.Interpreter.init(
        allocator,
        &initialEnv,
        stdout.any(),
        &bw,
        stdinBW.reader().any(),
        stderr.any(),
    );
    defer interpreter_.deinit();

    // Read builtin types
    const binDirName = std.fs.path.dirname(consoleArgs[0]) orelse ".";
    var binDir: std.fs.Dir = undefined;
    if (std.fs.path.isAbsolute(binDirName)) {
        binDir = try std.fs.openDirAbsolute(binDirName, .{});
    } else {
        binDir = try std.fs.cwd().openDir(binDirName, .{});
    }
    defer binDir.close();

    var builtin = binDir.openDir("../lib/matura-project/builtin", .{ .iterate = true }) catch |err| switch (err) {
        error.FileNotFound => {
            try stderr.print("Lib folder is not in the parent folder of the binary", .{});
            try errbw.flush();
            return 1;
        },
        else => {
            return err;
        },
    };
    defer builtin.close();
    var builtins = builtin.iterate();
    while (try builtins.next()) |builtinFile| {
        if (builtinFile.kind == .file) {
            var f = try builtin.openFile(builtinFile.name, .{ .mode = .read_only });
            defer f.close();
            const fileName = try std.fs.path.resolve(allocator, &.{
                binDirName,
                "../lib/matura-project/builtin",
                builtinFile.name,
            });
            if (try fileRunner.appendFile(
                f,
                fileName,
                &fileParser,
                &algorithmJ,
                &errs,
                &errbw,
            )) |returnCode| {
                return returnCode;
            }
        }
    }

    try fileParser.initBuiltins();

    // Read the standard library
    var stdlib = binDir.openDir("../lib/matura-project/stdlib", .{ .iterate = true }) catch |err| switch (err) {
        error.FileNotFound => {
            try stderr.print("Lib folder is not in the parent folder of the binary", .{});
            try errbw.flush();
            return 1;
        },
        else => {
            return err;
        },
    };
    defer stdlib.close();
    var libs = stdlib.iterate();
    while (try libs.next()) |libFile| {
        if (libFile.kind == .file) {
            var f = try stdlib.openFile(libFile.name, .{ .mode = .read_only });
            defer f.close();
            const fileName = try std.fs.path.resolve(allocator, &.{
                binDirName,
                "../lib/matura-project/stdlib",
                libFile.name,
            });
            if (try fileRunner.appendFile(
                f,
                fileName,
                &fileParser,
                &algorithmJ,
                &errs,
                &errbw,
            )) |returnCode| {
                return returnCode;
            }
        }
    }

    // Initialize file
    const cwd = std.fs.cwd();
    const file = cwd.openFile(consoleArgs[1], .{ .mode = .read_only }) catch |err| {
        switch (err) {
            error.FileNotFound => {
                try stderr.print("File {s} does not exist.\n", .{consoleArgs[1]});
                try errbw.flush();
                return 1;
            },
            else => {},
        }
        return err;
    };
    defer file.close();

    const fileName = try std.fs.path.resolve(allocator, &.{consoleArgs[1]});

    // Run the file
    if (try fileRunner.appendFile(
        file,
        fileName,
        &fileParser,
        &algorithmJ,
        &errs,
        &errbw,
    )) |returnCode| {
        return returnCode;
    }

    if (try fileRunner.checkStatements(
        &algorithmJ,
        &errs,
        stderr.any(),
        &errbw,
    )) |returnCode| {
        return returnCode;
    }

    try fileRunner.optimize(false);

    const compile = true;
    if (compile) {
        var monomorphizer = try monomorphization.Monomorphizer.init(
            allocator,
            &algorithmJ,
        );
        defer monomorphizer.deinit();
        try fileRunner.monomorphize(&monomorphizer);

        var compiler_ = try compiler.Compiler.init(allocator, &algorithmJ);
        defer compiler_.deinit();
        try compiler_.initBuiltins();
        try fileRunner.compile(&compiler_);

        var errorMessage: [*c]u8 = null;
        if (compiler.c.LLVMPrintModuleToFile(
            compiler_.module,
            "a.out.ll",
            &errorMessage,
        ) != 0) {
            try stderr.print("{s}", .{errorMessage});
            try errbw.flush();
            return 1;
        }
    } else {
        if (try fileRunner.run(&interpreter_, &errbw)) |returnCode| {
            return returnCode;
        }

        // Run the main function
        interpreter_.runMain() catch |err| switch (err) {
            error.Overflow, error.UnknownIdentifier => {
                try errbw.flush();
                return 1;
            },
            else => {
                return err;
            },
        };
        try bw.flush();
    }

    return 0;
}

// This test collects all the tests from imports
test {
    std.testing.refAllDecls(@This());
}
