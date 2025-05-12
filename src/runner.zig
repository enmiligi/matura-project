const std = @import("std");
const parser = @import("parser.zig");
const type_inference = @import("type_inference.zig");
const interpreter = @import("interpreter.zig");
const errors = @import("errors.zig");
const ast = @import("ast.zig");
const optimizer = @import("optimizer.zig");

pub const Runner = struct {
    sources: std.ArrayList([]const u8),
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) Runner {
        return .{
            .sources = .init(allocator),
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *Runner) void {
        for (self.sources.items) |source| {
            self.allocator.free(source);
        }
        self.sources.deinit();
    }

    pub fn runFile(
        self: *Runner,
        file: std.fs.File,
        fileName: []const u8,
        fileParser: *parser.Parser,
        algorithmJ: *type_inference.AlgorithmJ,
        interpreter_: *interpreter.Interpreter,
        errs: *errors.Errors,
        errbw: *std.io.BufferedWriter(4096, std.io.AnyWriter),
    ) !?u8 {
        const fileContents = try file.readToEndAlloc(self.allocator, std.math.maxInt(usize));
        {
            errdefer self.allocator.free(fileContents);
            try self.sources.append(fileContents);
        }
        errs.newSource(fileName, fileContents);
        try fileParser.newSource(fileContents);
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
        defer ast.Statement.deinitStatements(statements, self.allocator);
        algorithmJ.depth = 0;
        for (statements.items) |*statement| {
            algorithmJ.checkStatement(statement.*) catch |err| switch (err) {
                error.UnknownIdentifier,
                error.CouldNotUnify,
                error.InfiniteType,
                error.TooGeneral,
                error.NonExhaustiveMatch,
                => {
                    try errbw.flush();
                    return 1;
                },
                else => {
                    return err;
                },
            };
            try optimizer.optimizeStatement(statement, self.allocator);
        }
        for (statements.items) |statement| {
            try interpreter_.runStatement(statement);
        }
        return null;
    }
};
