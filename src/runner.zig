const std = @import("std");
const parser = @import("parser.zig");
const type_inference = @import("type_inference.zig");
const interpreter = @import("interpreter.zig");
const errors = @import("errors.zig");
const ast = @import("ast.zig");
const optimizer = @import("optimizer.zig");

pub const Runner = struct {
    sources: std.ArrayList([]const u8),
    statements: std.ArrayList(std.ArrayList(ast.Statement)),
    fileNames: std.ArrayList([]const u8),
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) Runner {
        return .{
            .sources = .init(allocator),
            .statements = .init(allocator),
            .fileNames = .init(allocator),
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *Runner) void {
        for (self.statements.items) |statements| {
            ast.Statement.deinitStatements(statements, self.allocator);
        }
        self.statements.deinit();
        for (self.sources.items) |source| {
            self.allocator.free(source);
        }
        self.sources.deinit();
        for (self.fileNames.items) |fileName| {
            self.allocator.free(fileName);
        }
        self.fileNames.deinit();
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
        // The file name has been allocated and needs to be freed
        try self.fileNames.append(fileName);
        const fileContents = try file.readToEndAlloc(self.allocator, std.math.maxInt(usize));
        {
            errdefer self.allocator.free(fileContents);
            try self.sources.append(fileContents);
        }
        // Change the source code of errors and the parser
        errs.newSource(fileName, fileContents);
        try fileParser.newSource(fileContents);

        // Set the depth to max to ensure that parsed annotations don't prevent generalization
        algorithmJ.depth = std.math.maxInt(usize);
        // Parse the file
        const statements = fileParser.file() catch |err| switch (err) {
            error.InvalidChar, error.UnexpectedToken, error.InvalidPrefix => {
                try errbw.flush();
                return 1;
            },
            else => {
                return err;
            },
        };
        self.statements.append(statements) catch |err| {
            ast.Statement.deinitStatements(statements, self.allocator);
            return err;
        };
        algorithmJ.depth = 0;

        // Type check the statements
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

        // Interpret the new file
        interpreter_.newFile(fileName);
        for (statements.items) |statement| {
            interpreter_.runStatement(statement) catch |err| switch (err) {
                error.Overflow, error.UnknownIdentifier => {
                    try errbw.flush();
                    return 1;
                },
                else => {
                    return err;
                },
            };
        }
        return null;
    }
};
