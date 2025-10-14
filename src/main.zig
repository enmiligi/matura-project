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

const compile = @import("config").compile;

const MainError = error{
    WrongUsage,
};

pub fn main() !u8 {
    // Initialize allocator for memory management
    var gpa = std.heap.GeneralPurposeAllocator(.{}).init;
    const allocator = gpa.allocator();
    defer _ = gpa.deinit();

    var stdoutBuffer: [2048]u8 = undefined;

    // Initialize stdin and stdout
    var stdout_writer = std.fs.File.stdout().writer(&stdoutBuffer);
    const stdout = &stdout_writer.interface;

    var stderrBuffer: [2048]u8 = undefined;

    var stderr_writer = std.fs.File.stderr().writer(&stderrBuffer);
    const stderr = &stderr_writer.interface;

    // Read commandline options
    const consoleArgs = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, consoleArgs);

    // Ensure correct usage
    if (consoleArgs.len != 2) {
        try stderr.print("Wrong number of arguments supplied.\n", .{});
        try stderr.print("Usage: matura-project file\n", .{});
        try stderr.flush();
        return MainError.WrongUsage;
    }

    // Create error handler
    var errs: errors.Errors = .{
        .stderr = stderr,
        .source = undefined,
        .fileName = undefined,
        .allocator = allocator,
        .typeVarMap = .init(allocator),
    };
    defer errs.deinit();

    // Get standard input
    var stdinBuffer: [2048]u8 = undefined;

    var stdin_reader = std.fs.File.stdin().reader(&stdinBuffer);
    const stdin = &stdin_reader.interface;

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
        stdout,
        stdin,
        stderr,
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
            try stderr.flush();
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
            try stderr.flush();
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
                try stderr.flush();
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
    )) |returnCode| {
        return returnCode;
    }

    const checkResult = try fileRunner.checkStatements(
        &algorithmJ,
        &errs,
        stderr,
    );

    var mainType = switch (checkResult) {
        .returnCode => |returnCode| {
            return returnCode;
        },
        .mainType => |mainType| mainType,
    };
    defer mainType.deinit(allocator);

    if (compile) {
        var monomorphizer = try monomorphization.Monomorphizer.init(
            allocator,
            &algorithmJ,
        );
        defer monomorphizer.deinit();
        const mainName = try fileRunner.monomorphize(
            &monomorphizer,
            mainType,
        );
        defer allocator.free(mainName);

        try fileRunner.optimize(false);

        var compiler_ = try compiler.Compiler.init(
            allocator,
            &algorithmJ,
            mainName,
        );
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
            try stderr.flush();
            return 1;
        }

        const baseName = std.fs.path.stem(fileName);

        const builtinsPath = try std.fs.path.join(
            allocator,
            &[_][]const u8{
                binDirName,
                "../lib/matura-project/builtinFunctions.c",
            },
        );
        defer allocator.free(builtinsPath);

        var cmd = std.process.Child.init(
            &[_][]const u8{
                "clang",
                builtinsPath,
                "a.out.ll",
                "-lm",
                "-lgc",
                "-O3",
                "-flto",
                "-o",
                baseName,
            },
            allocator,
        );
        try cmd.spawn();
        _ = try cmd.wait();

        try cwd.deleteFile("a.out.ll");
    } else {
        try fileRunner.optimize(false);

        if (try fileRunner.run(&interpreter_, stderr)) |returnCode| {
            return returnCode;
        }

        // Run the main function
        interpreter_.runMain() catch |err| switch (err) {
            error.Overflow, error.UnknownIdentifier => {
                try stderr.flush();
                return 1;
            },
            else => {
                return err;
            },
        };
        try stdout.flush();
    }

    return 0;
}

// This test collects all the tests from imports
test {
    std.testing.refAllDecls(@This());
}
