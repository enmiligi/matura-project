const std = @import("std");
const parser = @import("parser.zig");
const type_inference = @import("type_inference.zig");
const interpreter = @import("interpreter.zig");
const errors = @import("errors.zig");
const ast = @import("ast.zig");
const optimizer = @import("optimizer.zig");
const desugaring = @import("desugaring.zig");
const Monomorphizer = @import("monomorphization.zig").Monomorphizer;
const compiler = @import("compiler.zig");
const Compiler = compiler.Compiler;

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
        errs: *errors.Errors,
        stderr: std.io.AnyWriter,
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
            // Set the source of errors
            errs.newSource(self.fileNames.items[0], self.sources.items[0]);
        }
        // Type check the statements
        for (self.statements.items, 0..) |*statement, i| {
            while (i == startOfNextFile) {
                fileIndex += 1;
                errs.newSource(self.fileNames.items[fileIndex], self.sources.items[fileIndex]);
                if (fileIndex + 1 < self.startOfFiles.items.len) {
                    startOfNextFile = self.startOfFiles.items[fileIndex + 1];
                } else {
                    break;
                }
            }
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
        }

        // Check that a main function exists and has the type Void -> Void
        if (algorithmJ.globalTypes.get("main") == null) {
            try errs.printError("No 'main' defined", .{});
            try errbw.flush();
            return 1;
        }

        const vType = try type_inference.Type.init(self.allocator);
        vType.data = .{ .composite = .{ .name = "Void", .args = .init(self.allocator) } };
        const fType = type_inference.Type.init(self.allocator) catch |err| {
            vType.deinit(self.allocator);
            return err;
        };
        vType.rc += 1;
        fType.data = .{ .function = .{
            .from = vType,
            .to = vType,
        } };
        defer fType.deinit(self.allocator);

        var mainType: *type_inference.Type = undefined;
        switch (algorithmJ.globalTypes.get("main").?.*) {
            .type => |t| {
                t.rc += 1;
                mainType = t;
            },
            .forall => |*forall| mainType = try algorithmJ.instantiate(forall),
        }
        defer mainType.deinit(self.allocator);

        algorithmJ.unify(mainType, fType) catch |err| switch (err) {
            error.CouldNotUnify => {
                try stderr.print("The main function should have type Void -> Void, but it has type ", .{});
                var currentTypeVar: usize = 0;
                var typeVarMap = std.AutoHashMap(usize, usize).init(self.allocator);
                defer typeVarMap.deinit();

                try type_inference.printType(
                    mainType,
                    stderr,
                    &currentTypeVar,
                    &typeVarMap,
                    true,
                    self.allocator,
                );
                try errbw.flush();
                return 1;
            },
            else => {
                return err;
            },
        };

        return null;
    }

    pub fn optimize(self: *Runner, interpreted: bool) !void {
        for (self.statements.items) |*statement| {
            try optimizer.optimizeStatement(statement, self.allocator, interpreted);
        }
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

    pub fn monomorphize(self: *Runner, monomorphizer: *Monomorphizer) !void {
        try Monomorphizer.instantiate(self.allocator, &self.statements);
        try monomorphizer.monomorphize(&self.statements);
    }

    pub fn compile(self: *Runner, compiler_: *Compiler) !void {
        try compiler_.compile(&self.statements);
    }
};
