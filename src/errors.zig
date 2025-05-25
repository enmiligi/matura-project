const std = @import("std");
const Token = @import("./token.zig").Token;
const type_inference = @import("./type_inference.zig");
const AST = @import("./ast.zig").AST;
const utils = @import("./utils.zig");

pub const Errors = struct {
    fileName: []const u8,
    source: []const u8,
    stderr: std.io.AnyWriter,
    allocator: std.mem.Allocator,
    errorOcurred: bool = false,

    typeVarMap: std.AutoHashMap(usize, usize),

    pub fn deinit(self: *Errors) void {
        self.typeVarMap.deinit();
    }

    pub fn newSource(self: *Errors, fileName: []const u8, source: []const u8) void {
        self.fileName = fileName;
        self.source = source;
    }

    // Indicate a subsequence with carets (^)
    fn printIndicator(self: *Errors, startIndex: usize, start: usize, end: usize) !void {
        const contents = try self.allocator.alloc(u8, end);
        defer self.allocator.free(contents);
        var i: usize = 0;
        var allWhitespace: bool = true;
        while (i < end) : (i += 1) {
            if (i < start) {
                contents[i] = ' ';
            } else if (self.source[startIndex + i] == ' ' and allWhitespace) {
                contents[i] = ' ';
            } else {
                allWhitespace = false;
                contents[i] = '^';
            }
        }
        try self.printYellow("{s}\n", .{contents});
    }

    // Print the first and last line of code of the Region
    // and indicate the subsequence with carets (^)
    fn indicateRegion(self: *Errors, region: utils.Region, comptime msg: []const u8, args: anytype, err: bool) !void {
        var startLine: usize = 0;
        var startOfLine: usize = 0;
        var endOfLine: usize = 0;
        var i: usize = 0;
        while (i < region.start and i != self.source.len) : (i += 1) {
            if (self.source[i] == '\n') {
                startLine += 1;
                startOfLine = i + 1;
            }
        }
        while (i != self.source.len and self.source[i] != '\n') {
            i += 1;
        }
        endOfLine = i;
        var endLine: usize = startLine;
        var startOfEndLine = i + 1;
        while (i < region.end and i != self.source.len) : (i += 1) {
            if (self.source[i] == '\n') {
                endLine += 1;
                startOfEndLine = i + 1;
            }
        }
        while (i != self.source.len and self.source[i] != '\n') {
            i += 1;
        }
        try self.printBold("{s}:{d}:{d}: ", .{
            self.fileName,
            startLine + 1,
            (region.start + 1) - startOfLine,
        });
        if (err) {
            try self.printError(msg, args);
        } else {
            try self.printBold(msg, args);
        }
        try self.stderr.print("\n{s}\n", .{self.source[startOfLine..endOfLine]});
        try self.printIndicator(startOfLine, region.start - startOfLine, @min(endOfLine, region.end) - startOfLine);
        if (startLine != endLine) {
            if (endLine - startLine > 1) {
                try self.stderr.print("    ...\n", .{});
            }
            try self.stderr.print("{s}\n", .{self.source[startOfEndLine..i]});
            try self.printIndicator(startOfEndLine, 0, @min(i, region.end) - startOfEndLine);
        }
    }

    fn indicateAST(self: *Errors, ast: *AST, comptime msg: []const u8, args: anytype, err: bool) !void {
        try self.indicateRegion(utils.computeBoundaries(ast), msg, args, err);
    }

    // Errors are printed in red and in bold
    pub fn printError(self: *Errors, comptime msg: []const u8, args: anytype) !void {
        try self.printBold("\x1b[31merror: \x1b[39m", .{});
        try self.printBold(msg, args);
    }

    fn printYellow(self: *Errors, comptime msg: []const u8, args: anytype) !void {
        try self.stderr.print("\x1b[33m", .{});
        try self.printBold(msg, args);
        try self.stderr.print("\x1b[39m", .{});
    }

    fn printBold(self: *Errors, comptime msg: []const u8, args: anytype) !void {
        try self.stderr.print("\x1b[1m", .{});
        try self.stderr.print(msg, args);
        try self.stderr.print("\x1b[22m", .{});
    }

    fn printConstraints(self: *Errors, t: *type_inference.Type, writer: std.io.AnyWriter, currentTypeVar: *usize) !void {
        try writer.print("\x1b[32;1m", .{});
        try type_inference.printConstraints(t, writer, currentTypeVar, &self.typeVarMap, self.allocator);
        try writer.print("\x1b[32m", .{});
    }

    // Types are written in bold
    // and yellow if it matches and red for the mismatched subtype
    fn printType(self: *Errors, t: *type_inference.Type, writer: std.io.AnyWriter, currentTypeVar: *usize, err: bool, topLevel: bool) !void {
        if (err) {
            try writer.print("\x1b[33;1m", .{});
        } else {
            try writer.print("\x1b[32;1m", .{});
        }
        try type_inference.printTypeWithoutConstraints(t, writer, currentTypeVar, &self.typeVarMap, topLevel, self.allocator);
        try writer.print("\x1b[32m", .{});
    }

    // For two types, print them to their corresponding writer
    // while changing to error style if they mismatch
    fn compareTypes(
        self: *Errors,
        leftType: *type_inference.Type,
        rightType: *type_inference.Type,
        leftWriter: std.io.AnyWriter,
        rightWriter: std.io.AnyWriter,
        currentTypeVar: *usize,
        topLevel: bool,
    ) !void {
        switch (leftType.data) {
            .primitive => |prim1| {
                switch (rightType.data) {
                    .primitive => |prim2| {
                        try self.printType(leftType, leftWriter, currentTypeVar, prim1 != prim2, topLevel);
                        try self.printType(rightType, rightWriter, currentTypeVar, prim1 != prim2, topLevel);
                    },
                    .typeVar => |typeVar2| {
                        if (typeVar2.subst) |subst| {
                            try self.compareTypes(leftType, subst, leftWriter, rightWriter, currentTypeVar, topLevel);
                        } else {
                            try self.printType(rightType, rightWriter, currentTypeVar, false, topLevel);
                            try self.printType(leftType, leftWriter, currentTypeVar, false, topLevel);
                        }
                    },
                    .function => {
                        try self.printType(leftType, leftWriter, currentTypeVar, true, topLevel);
                        try self.printType(rightType, rightWriter, currentTypeVar, true, topLevel);
                    },
                    .number => |*num2| {
                        switch (num2.variable.data) {
                            .typeVar => |tV| {
                                if (tV.subst) |subst| {
                                    subst.rc += 1;
                                    num2.variable.deinit(self.allocator);
                                    num2.variable = subst;
                                    try self.compareTypes(leftType, rightType, leftWriter, rightWriter, currentTypeVar, topLevel);
                                } else {
                                    switch (prim1) {
                                        .Int, .Float => {
                                            try self.printType(leftType, leftWriter, currentTypeVar, false, topLevel);
                                            try self.printType(rightType, rightWriter, currentTypeVar, false, topLevel);
                                        },
                                        .Bool, .Char => {
                                            try self.printType(leftType, leftWriter, currentTypeVar, true, topLevel);
                                            try self.printType(rightType, rightWriter, currentTypeVar, true, topLevel);
                                        },
                                    }
                                }
                            },
                            .primitive => |prim2| {
                                try self.printType(leftType, leftWriter, currentTypeVar, prim1 != prim2, topLevel);
                                try self.printType(rightType, rightWriter, currentTypeVar, prim1 != prim2, topLevel);
                            },
                            else => {},
                        }
                    },
                    .composite => {
                        try self.printType(leftType, leftWriter, currentTypeVar, true, topLevel);
                        try self.printType(rightType, rightWriter, currentTypeVar, true, topLevel);
                    },
                }
            },
            .typeVar => |tV1| {
                if (tV1.subst) |subst| {
                    try self.compareTypes(subst, rightType, leftWriter, rightWriter, currentTypeVar, topLevel);
                } else {
                    try self.printType(leftType, leftWriter, currentTypeVar, false, topLevel);
                    try self.printType(rightType, rightWriter, currentTypeVar, false, topLevel);
                }
            },
            .function => |fun1| {
                switch (rightType.data) {
                    .function => |fun2| {
                        if (!topLevel) {
                            try leftWriter.print("(", .{});
                            try rightWriter.print("(", .{});
                        }
                        try self.compareTypes(fun1.from, fun2.from, leftWriter, rightWriter, currentTypeVar, false);
                        try leftWriter.print(" -> ", .{});
                        try rightWriter.print(" -> ", .{});
                        try self.compareTypes(fun1.to, fun2.to, leftWriter, rightWriter, currentTypeVar, true);
                        if (!topLevel) {
                            try leftWriter.print(")", .{});
                            try rightWriter.print(")", .{});
                        }
                    },
                    .typeVar => |tV2| {
                        if (tV2.subst) |subst| {
                            try self.compareTypes(leftType, subst, leftWriter, rightWriter, currentTypeVar, topLevel);
                        } else {
                            try self.printType(leftType, leftWriter, currentTypeVar, false, topLevel);
                            try self.printType(rightType, rightWriter, currentTypeVar, false, topLevel);
                        }
                    },
                    else => {
                        try self.printType(leftType, leftWriter, currentTypeVar, true, topLevel);
                        try self.printType(rightType, rightWriter, currentTypeVar, true, topLevel);
                    },
                }
            },
            .number => |*num1| {
                switch (num1.variable.data) {
                    .typeVar => |tV| {
                        if (tV.subst) |subst| {
                            subst.rc += 1;
                            num1.variable.deinit(self.allocator);
                            num1.variable = subst;
                            try self.compareTypes(leftType, rightType, leftWriter, rightWriter, currentTypeVar, topLevel);
                        } else {
                            switch (rightType.data) {
                                .typeVar => |tV2| {
                                    if (tV2.subst) |subst| {
                                        try self.compareTypes(leftType, subst, leftWriter, rightWriter, currentTypeVar, topLevel);
                                    } else {
                                        try self.printType(leftType, leftWriter, currentTypeVar, false, topLevel);
                                        try self.printType(rightType, rightWriter, currentTypeVar, false, topLevel);
                                    }
                                },
                                .primitive => |prim1| {
                                    switch (prim1) {
                                        .Int, .Float => {
                                            try self.printType(leftType, leftWriter, currentTypeVar, false, topLevel);
                                            try self.printType(rightType, rightWriter, currentTypeVar, false, topLevel);
                                        },
                                        .Bool, .Char => {
                                            try self.printType(leftType, leftWriter, currentTypeVar, true, topLevel);
                                            try self.printType(rightType, rightWriter, currentTypeVar, true, topLevel);
                                        },
                                    }
                                },
                                else => {
                                    try self.printType(leftType, leftWriter, currentTypeVar, true, topLevel);
                                    try self.printType(rightType, rightWriter, currentTypeVar, true, topLevel);
                                },
                            }
                        }
                    },
                    .primitive => {
                        try self.compareTypes(num1.variable, rightType, leftWriter, rightWriter, currentTypeVar, topLevel);
                    },
                    else => {},
                }
            },
            .composite => |composite1| {
                switch (rightType.data) {
                    .typeVar => |tV2| {
                        if (tV2.subst) |subst| {
                            try self.compareTypes(leftType, subst, leftWriter, rightWriter, currentTypeVar, topLevel);
                        } else {
                            try self.printType(leftType, leftWriter, currentTypeVar, false, topLevel);
                            try self.printType(rightType, rightWriter, currentTypeVar, false, topLevel);
                        }
                    },
                    .composite => |composite2| {
                        if (!std.mem.eql(u8, composite1.name, composite2.name)) {
                            try self.printType(leftType, leftWriter, currentTypeVar, true, topLevel);
                            try self.printType(rightType, rightWriter, currentTypeVar, true, topLevel);
                        } else {
                            if (!topLevel) {
                                try leftWriter.print("(", .{});
                                try rightWriter.print("(", .{});
                            }
                            try leftWriter.print("{s}", .{composite1.name});
                            try rightWriter.print("{s}", .{composite2.name});
                            var i: usize = 0;
                            while (i < composite1.args.items.len) : (i += 1) {
                                try leftWriter.print(" ", .{});
                                try rightWriter.print(" ", .{});
                                try self.compareTypes(
                                    composite1.args.items[i],
                                    composite2.args.items[i],
                                    leftWriter,
                                    rightWriter,
                                    currentTypeVar,
                                    false,
                                );
                            }
                            if (!topLevel) {
                                try leftWriter.print(")", .{});
                                try rightWriter.print(")", .{});
                            }
                        }
                    },
                    else => {
                        try self.printType(leftType, leftWriter, currentTypeVar, true, topLevel);
                        try self.printType(rightType, rightWriter, currentTypeVar, true, topLevel);
                    },
                }
            },
        }
    }

    // For two types, compare the types generating two strings
    // which are red at the mismatching subtype
    fn stringCompareTypes(
        self: *Errors,
        leftType: *type_inference.Type,
        rightType: *type_inference.Type,
    ) !struct { left: std.ArrayList(u8), right: std.ArrayList(u8) } {
        var leftString = std.ArrayList(u8).init(self.allocator);
        var rightString = std.ArrayList(u8).init(self.allocator);
        var currentTypeVar: usize = 0;
        var leftWriter = leftString.writer();
        var rightWriter = rightString.writer();
        try leftWriter.print("\x1b[32m", .{});
        try rightWriter.print("\x1b[32m", .{});
        try self.printConstraints(leftType, leftWriter.any(), &currentTypeVar);
        currentTypeVar = 0;
        try self.printConstraints(rightType, rightWriter.any(), &currentTypeVar);
        try self.compareTypes(leftType, rightType, leftWriter.any(), rightWriter.any(), &currentTypeVar, true);
        try leftWriter.print("\x1b[39m", .{});
        try rightWriter.print("\x1b[39m", .{});
        return .{ .left = leftString, .right = rightString };
    }

    // This error is printed when the type of a recursive value
    // seems to be infinitely large
    pub fn recursionInfiniteType(
        self: *Errors,
        ast: *AST,
        varName: []const u8,
        leftType: *type_inference.Type,
        rightType: *type_inference.Type,
    ) !void {
        const comparedTypes = try self.stringCompareTypes(leftType, rightType);
        defer comparedTypes.left.deinit();
        defer comparedTypes.right.deinit();
        try self.indicateAST(
            ast,
            "The type of {s} when recursing seems to be {s},\nbut {s} is the type inferred when set,\nwhich leads to an infinite type",
            .{ varName, comparedTypes.left.items, comparedTypes.right.items },
            true,
        );
    }

    // Error for when the type in the recursion is another than the complete type
    pub fn recursionTwoTypes(
        self: *Errors,
        ast: *AST,
        varName: []const u8,
        leftType: *type_inference.Type,
        rightType: *type_inference.Type,
    ) !void {
        const comparedTypes = try self.stringCompareTypes(leftType, rightType);
        defer comparedTypes.left.deinit();
        defer comparedTypes.right.deinit();
        try self.indicateAST(
            ast,
            "The type of {s} when recursing seems to be {s},\nbut {s} is the type it has",
            .{ varName, comparedTypes.left.items, comparedTypes.right.items },
            true,
        );
    }

    // This error is used when two types should be equal for various reasons
    pub fn typeComparison(
        self: *Errors,
        codeRegion: utils.Region,
        leftType: *type_inference.Type,
        rightType: *type_inference.Type,
        err: []const u8,
        reason: []const u8,
        reasonAt: utils.Region,
    ) !void {
        const comparedTypes = try self.stringCompareTypes(leftType, rightType);
        defer comparedTypes.left.deinit();
        defer comparedTypes.right.deinit();
        try self.indicateRegion(codeRegion, "The type of this is: {s}", .{comparedTypes.left.items}, true);
        try self.printBold("which {s}: {s},\nbecause {s}\n", .{ err, comparedTypes.right.items, reason });
        try self.indicateRegion(reasonAt, "", .{}, false);
    }

    pub fn tooGeneralArgumentType(
        self: *Errors,
        ast: *AST,
        argumentType: *type_inference.Type,
    ) !void {
        try self.indicateRegion(ast.lambda.argType.?.region, "This type is annotated", .{}, true);
        try self.indicateRegion(
            .{ .start = ast.lambda.argname.start, .end = ast.lambda.argname.end },
            "which is more general than the type of this argument",
            .{},
            false,
        );
        try self.stderr.print(", which is: ", .{});
        var currentTypeVar: usize = 0;
        try self.printConstraints(argumentType, self.stderr, &currentTypeVar);
        try self.printType(argumentType, self.stderr, &currentTypeVar, false, true);
        try self.stderr.print("\n", .{});
    }

    pub fn tooGeneralVariableType(
        self: *Errors,
        ast: *AST,
        variableType: *type_inference.Type,
        annotatedRegion: utils.Region,
    ) !void {
        try self.indicateRegion(annotatedRegion, "This type is annotated", .{}, true);
        try self.indicateRegion(
            utils.computeBoundaries(ast),
            "which is more general than the type of this argument",
            .{},
            false,
        );
        try self.stderr.print(", which is: ", .{});
        var currentTypeVar: usize = 0;
        try self.printConstraints(variableType, self.stderr, &currentTypeVar);
        try self.printType(variableType, self.stderr, &currentTypeVar, false, true);
        try self.stderr.print("\n", .{});
    }

    // Error for when two expressions should have the same type, but don't
    pub fn typeMismatch(
        self: *Errors,
        astLeft: *AST,
        astRight: *AST,
        leftType: *type_inference.Type,
        rightType: *type_inference.Type,
        reasonWhyEqual: []const u8,
    ) !void {
        const comparedTypes = try self.stringCompareTypes(leftType, rightType);
        defer comparedTypes.left.deinit();
        defer comparedTypes.right.deinit();
        try self.indicateAST(astLeft, "The type of this is: {s}", .{comparedTypes.left.items}, true);
        try self.indicateAST(astRight, "which should match this type: {s}", .{comparedTypes.right.items}, false);
        try self.printBold("because {s}\n", .{reasonWhyEqual});
    }

    // Print an error at a specific point in the code
    pub fn errorAt(self: *Errors, start: usize, end: usize, comptime msg: []const u8, args: anytype) !void {
        try self.indicateRegion(.{ .start = start, .end = end }, msg, args, true);
    }
};
