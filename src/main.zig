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
pub const runner = @import("./runner.zig");

const MainError = error{
    WrongUsage,
};

pub fn main() !u8 {
    // Initialize allocator for memory management
    var gpa = std.heap.GeneralPurposeAllocator(.{ .stack_trace_frames = 20 }).init;
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

    var errs: errors.Errors = .{
        .stderr = stderr.any(),
        .source = undefined,
        .fileName = undefined,
        .allocator = allocator,
        .typeVarMap = .init(allocator),
    };
    defer errs.deinit();

    const stdin = std.io.getStdIn().reader();
    var stdinBW = std.io.bufferedReader(stdin);

    var fileRunner = runner.Runner.init(allocator);

    defer fileRunner.deinit();

    var algorithmJ = type_inference.AlgorithmJ.init(allocator, &errs);
    defer algorithmJ.deinit();

    // Create Parser
    var fileParser = try parser.Parser.init(allocator, &errs, &algorithmJ);
    defer fileParser.deinit();

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
            if (try fileRunner.runFile(
                f,
                fileName,
                &fileParser,
                &algorithmJ,
                &interpreter_,
                &errs,
                &errbw,
            )) |returnCode| {
                return returnCode;
            }
        }
    }

    try fileParser.initBuiltins();

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
            if (try fileRunner.runFile(
                f,
                fileName,
                &fileParser,
                &algorithmJ,
                &interpreter_,
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

    if (try fileRunner.runFile(
        file,
        fileName,
        &fileParser,
        &algorithmJ,
        &interpreter_,
        &errs,
        &errbw,
    )) |returnCode| {
        return returnCode;
    }

    if (algorithmJ.globalTypes.get("main") == null) {
        try errs.printError("No 'main' defined", .{});
        try errbw.flush();
        return 1;
    }

    const vType = try type_inference.Type.init(allocator);
    vType.data = .{ .composite = .{ .name = "Void", .args = .init(allocator) } };
    const fType = type_inference.Type.init(allocator) catch |err| {
        vType.deinit(allocator);
        return err;
    };
    vType.rc += 1;
    fType.data = .{ .function = .{ .from = vType, .to = vType } };
    defer fType.deinit(allocator);

    var mainType: *type_inference.Type = undefined;
    switch (algorithmJ.globalTypes.get("main").?.*) {
        .type => |t| {
            t.rc += 1;
            mainType = t;
        },
        .forall => |*forall| mainType = try algorithmJ.instantiate(forall),
    }
    defer mainType.deinit(allocator);

    algorithmJ.unify(mainType, fType) catch |err| switch (err) {
        error.CouldNotUnify => {
            try stderr.print("The main function should have type Void -> Void, but it has type ", .{});
            var currentTypeVar: usize = 0;
            var typeVarMap = std.AutoHashMap(usize, usize).init(allocator);
            defer typeVarMap.deinit();

            try type_inference.printType(
                mainType,
                stderr.any(),
                &currentTypeVar,
                &typeVarMap,
                true,
                allocator,
            );
            try errbw.flush();
            return 1;
        },
        else => {
            return err;
        },
    };

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

    return 0;
}

// This test collects all the tests from imports
test {
    std.testing.refAllDecls(@This());
}
