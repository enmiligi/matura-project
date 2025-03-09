const std = @import("std");
const Token = @import("./token.zig").Token;
const type_inference = @import("./type_inference.zig");
const AST = @import("./ast.zig").AST;

const Region = struct {
    start: usize,
    end: usize,
};

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

    pub fn computeBoundaries(ast: *AST) Region {
        switch (ast.*) {
            .intConstant => |iC| {
                return .{ .start = iC.token.start, .end = iC.token.end };
            },
            .floatConstant => |fC| {
                return .{ .start = fC.token.start, .end = fC.token.end };
            },
            .lambda => |lambda| {
                const end = computeBoundaries(lambda.expr).end;
                return .{ .start = lambda.start, .end = end };
            },
            .call => |call| {
                const end = computeBoundaries(call.arg).end;
                const start = computeBoundaries(call.function).start;
                return .{ .start = start, .end = end };
            },
            .let => |let| {
                const end = computeBoundaries(let.in).end;
                return .{ .start = let.start, .end = end };
            },
            .identifier => |id| {
                return .{ .start = id.token.start, .end = id.token.end };
            },
            .operator => |op| {
                const start = computeBoundaries(op.left).start;
                const end = computeBoundaries(op.right).end;
                return .{ .start = start, .end = end };
            },
        }
    }

    fn indicateRegion(self: *Errors, region: Region, comptime msg: []const u8, args: anytype, err: bool) !void {
        var startLine: usize = 0;
        var startOfLine: usize = 0;
        var endOfLine: usize = 0;
        var i: usize = 0;
        while (i <= region.start) : (i += 1) {
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
        while (i < region.end) : (i += 1) {
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
            region.start - startOfLine + 1,
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
        try self.indicateRegion(computeBoundaries(ast), msg, args, err);
    }

    fn printError(self: *Errors, comptime msg: []const u8, args: anytype) !void {
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

    fn printType(self: *Errors, t: *type_inference.Type, writer: std.io.AnyWriter, currentTypeVar: *usize, err: bool) !void {
        if (err) {
            try writer.print("\x1b[33;1m", .{});
        } else {
            try writer.print("\x1b[32;1m", .{});
        }
        try type_inference.printType(t, writer, currentTypeVar, &self.typeVarMap);
        try writer.print("\x1b[32m", .{});
    }

    fn compareTypes(
        self: *Errors,
        leftType: *type_inference.Type,
        rightType: *type_inference.Type,
        leftWriter: std.io.AnyWriter,
        rightWriter: std.io.AnyWriter,
        currentTypeVar: *usize,
    ) !void {
        switch (leftType.data) {
            .primitive => |prim1| {
                switch (rightType.data) {
                    .primitive => |prim2| {
                        try self.printType(leftType, leftWriter, currentTypeVar, prim1 != prim2);
                        try self.printType(rightType, rightWriter, currentTypeVar, prim1 != prim2);
                    },
                    .typeVar => |typeVar2| {
                        if (typeVar2.subst) |subst| {
                            try self.compareTypes(leftType, subst, leftWriter, rightWriter, currentTypeVar);
                        } else {
                            try self.printType(rightType, rightWriter, currentTypeVar, false);
                            try self.printType(leftType, leftWriter, currentTypeVar, false);
                        }
                    },
                    .function => {
                        try self.printType(leftType, leftWriter, currentTypeVar, true);
                        try self.printType(rightType, rightWriter, currentTypeVar, true);
                    },
                    .number => |*num2| {
                        switch (num2.variable.data) {
                            .typeVar => |tV| {
                                if (tV.subst) |subst| {
                                    subst.rc += 1;
                                    num2.variable.deinit(self.allocator);
                                    num2.variable = subst;
                                    try self.compareTypes(leftType, rightType, leftWriter, rightWriter, currentTypeVar);
                                } else {
                                    switch (prim1) {
                                        .Int, .Float => {
                                            try self.printType(leftType, leftWriter, currentTypeVar, false);
                                            try self.printType(rightType, rightWriter, currentTypeVar, false);
                                        },
                                    }
                                }
                            },
                            .primitive => |prim2| {
                                try self.printType(leftType, leftWriter, currentTypeVar, prim1 != prim2);
                                try self.printType(rightType, rightWriter, currentTypeVar, prim1 != prim2);
                            },
                            else => {},
                        }
                    },
                }
            },
            .typeVar => |tV1| {
                if (tV1.subst) |subst| {
                    try self.compareTypes(subst, rightType, leftWriter, rightWriter, currentTypeVar);
                } else {
                    try self.printType(leftType, leftWriter, currentTypeVar, false);
                    try self.printType(rightType, rightWriter, currentTypeVar, false);
                }
            },
            .function => |fun1| {
                switch (rightType.data) {
                    .function => |fun2| {
                        try leftWriter.print("(", .{});
                        try rightWriter.print("(", .{});
                        try self.compareTypes(fun1.from, fun2.from, leftWriter, rightWriter, currentTypeVar);
                        try leftWriter.print(" -> ", .{});
                        try rightWriter.print(" -> ", .{});
                        try self.compareTypes(fun1.to, fun2.to, leftWriter, rightWriter, currentTypeVar);
                        try leftWriter.print(")", .{});
                        try rightWriter.print(")", .{});
                    },
                    .typeVar => |tV2| {
                        if (tV2.subst) |subst| {
                            try self.compareTypes(leftType, subst, leftWriter, rightWriter, currentTypeVar);
                        } else {
                            try self.printType(leftType, leftWriter, currentTypeVar, false);
                            try self.printType(rightType, rightWriter, currentTypeVar, false);
                        }
                    },
                    else => {
                        try self.printType(leftType, leftWriter, currentTypeVar, true);
                        try self.printType(rightType, rightWriter, currentTypeVar, true);
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
                            try self.compareTypes(leftType, rightType, leftWriter, rightWriter, currentTypeVar);
                        } else {
                            switch (rightType.data) {
                                .typeVar => |tV2| {
                                    if (tV2.subst) |subst| {
                                        try self.compareTypes(leftType, subst, leftWriter, rightWriter, currentTypeVar);
                                    } else {
                                        try self.printType(leftType, leftWriter, currentTypeVar, false);
                                        try self.printType(rightType, rightWriter, currentTypeVar, false);
                                    }
                                },
                                .primitive => |prim1| {
                                    switch (prim1) {
                                        .Int, .Float => {
                                            try self.printType(leftType, leftWriter, currentTypeVar, false);
                                            try self.printType(rightType, rightWriter, currentTypeVar, false);
                                        },
                                    }
                                },
                                else => {
                                    try self.printType(leftType, leftWriter, currentTypeVar, true);
                                    try self.printType(rightType, rightWriter, currentTypeVar, true);
                                },
                            }
                        }
                    },
                    .primitive => {
                        try self.compareTypes(num1.variable, rightType, leftWriter, rightWriter, currentTypeVar);
                    },
                    else => {},
                }
            },
        }
    }

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
        try self.compareTypes(leftType, rightType, leftWriter.any(), rightWriter.any(), &currentTypeVar);
        try leftWriter.print("\x1b[39m", .{});
        try rightWriter.print("\x1b[39m", .{});
        return .{ .left = leftString, .right = rightString };
    }

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

    pub fn typeComparison(
        self: *Errors,
        ast: *AST,
        leftType: *type_inference.Type,
        rightType: *type_inference.Type,
        err: []const u8,
        reason: []const u8,
        reasonAt: Region,
    ) !void {
        const comparedTypes = try self.stringCompareTypes(leftType, rightType);
        defer comparedTypes.left.deinit();
        defer comparedTypes.right.deinit();
        try self.indicateAST(ast, "The type of this is: {s}", .{comparedTypes.left.items}, true);
        try self.printBold("which {s}: {s},\nbecause {s}\n", .{ err, comparedTypes.right.items, reason });
        try self.indicateRegion(reasonAt, "", .{}, false);
    }

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

    pub fn errorAt(self: *Errors, start: usize, end: usize, comptime msg: []const u8, args: anytype) !void {
        var line: usize = 0;
        var startOfLine: usize = 0;
        var i: usize = 0;
        while (i <= start) : (i += 1) {
            if (self.source[i] == '\n') {
                line += 1;
                startOfLine = i + 1;
            }
        }
        while (i != self.source.len and self.source[i] != '\n') {
            i += 1;
        }
        try self.stderr.print("\x1b[1m{s}:{d}:{d}:\x1b[m ", .{
            self.fileName,
            line + 1,
            start - startOfLine + 1,
        });
        try self.printError(msg, args);
        try self.stderr.print("\n{s}\n", .{self.source[startOfLine..i]});
        try self.printIndicator(startOfLine, start - startOfLine, end - startOfLine);
    }
};
