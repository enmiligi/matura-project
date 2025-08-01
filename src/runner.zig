const std = @import("std");
const parser = @import("parser.zig");
const type_inference = @import("type_inference.zig");
const interpreter = @import("interpreter.zig");
const errors = @import("errors.zig");
const ast = @import("ast.zig");
const optimizer = @import("optimizer.zig");
const desugaring = @import("desugaring.zig");

pub const Runner = struct {
    sources: std.ArrayList([]const u8),
    statements: std.ArrayList(ast.Statement),
    startOfFiles: std.ArrayList(usize),
    fileNames: std.ArrayList([]const u8),
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) Runner {
        return .{
            .sources = .init(allocator),
            .statements = .init(allocator),
            .fileNames = .init(allocator),
            .startOfFiles = .init(allocator),
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *Runner) void {
        ast.Statement.deinitStatements(self.statements, self.allocator);
        for (self.sources.items) |source| {
            self.allocator.free(source);
        }
        self.sources.deinit();
        for (self.fileNames.items) |fileName| {
            self.allocator.free(fileName);
        }
        self.fileNames.deinit();
        self.startOfFiles.deinit();
    }

    pub fn appendFile(
        self: *Runner,
        file: std.fs.File,
        fileName: []const u8,
        fileParser: *parser.Parser,
        algorithmJ: *type_inference.AlgorithmJ,
        errs: *errors.Errors,
        errbw: *std.io.BufferedWriter(4096, std.io.AnyWriter),
    ) !?u8 {
        // The file name has been allocated and needs to be freed
        try self.fileNames.append(fileName);
        try self.startOfFiles.append(self.statements.items.len);
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
        fileParser.appendFile(&self.statements) catch |err| switch (err) {
            error.InvalidChar, error.UnexpectedToken, error.InvalidPrefix => {
                try errbw.flush();
                return 1;
            },
            else => {
                return err;
            },
        };
        algorithmJ.depth = 0;
        return null;
    }

    pub fn checkStatements(
        self: *Runner,
        algorithmJ: *type_inference.AlgorithmJ,
        errbw: *std.io.BufferedWriter(4096, std.io.AnyWriter),
        interpreted: bool,
    ) !?u8 {
        // Type check the statements
        for (self.statements.items) |*statement| {
            algorithmJ.checkStatement(statement) catch |err| switch (err) {
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
            try desugaring.desugarStatement(statement, self.allocator);
            try optimizer.optimizeStatement(statement, self.allocator, interpreted);
        }

        return null;
    }

    pub fn run(
        self: *Runner,
        interpreter_: *interpreter.Interpreter,
        errbw: *std.io.BufferedWriter(4096, std.io.AnyWriter),
    ) !?u8 {
        var startOfNextFile: usize = undefined;
        if (self.startOfFiles.items.len > 1) {
            startOfNextFile = self.startOfFiles.items[1];
        } else {
            startOfNextFile = std.math.maxInt(usize);
        }
        var fileIndex: usize = 0;
        if (self.startOfFiles.items.len != 0) {
            // Interpret the new file
            interpreter_.newFile(self.fileNames.items[0]);
        }
        for (self.statements.items, 0..) |statement, i| {
            while (i == startOfNextFile) {
                fileIndex += 1;
                interpreter_.newFile(self.fileNames.items[fileIndex]);
                if (fileIndex + 1 < self.startOfFiles.items.len) {
                    startOfNextFile = self.startOfFiles.items[fileIndex + 1];
                } else {
                    break;
                }
            }
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
