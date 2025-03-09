const std = @import("std");
const Token = @import("./token.zig").Token;
const type_inference = @import("./type_inference.zig");
const AST = @import("./ast.zig").AST;

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

    fn printIndicator(self: *Errors, start: usize, end: usize) !void {
        const leftPad = try self.allocator.alloc(u8, start);
        defer self.allocator.free(leftPad);
        @memset(leftPad, ' ');
        const indicator = try self.allocator.alloc(u8, end - start);
        defer self.allocator.free(indicator);
        @memset(indicator, '^');
        try self.stderr.print("{s}\x1b[33m{s}\x1b[m\n", .{ leftPad, indicator });
    }

    fn printError(self: *Errors, msg: []const u8) !void {
        try self.stderr.print("\x1b[31;1merror: {s}\x1b[m", .{msg});
    }

    fn printType(self: *Errors, t: *type_inference.Type, writer: std.io.AnyWriter, currentTypeVar: *usize, err: bool) !void {
        if (err) try writer.print("\x1b[33;1m", .{});
        try type_inference.printType(t, writer, currentTypeVar, &self.typeVarMap);
        if (err) try writer.print("\x1b[m", .{});
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

    pub fn typeMismatch(
        self: *Errors,
        astLeft: *AST,
        astRight: *AST,
        leftType: *type_inference.Type,
        rightType: *type_inference.Type,
        reasonWhyEqual: []const u8,
    ) !void {
        var leftString = std.ArrayList(u8).init(self.allocator);
        defer leftString.deinit();
        var rightString = std.ArrayList(u8).init(self.allocator);
        defer rightString.deinit();
        var currentTypeVar: usize = 0;
        _ = astLeft;
        _ = astRight;
        _ = reasonWhyEqual;
        try self.compareTypes(leftType, rightType, leftString.writer().any(), rightString.writer().any(), &currentTypeVar);
        try self.stderr.print("{s}\n", .{leftString.items});
        try self.stderr.print("{s}\n", .{rightString.items});
    }

    pub fn errorAt(self: *Errors, start: usize, end: usize, msg: []const u8) !void {
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
        try self.stderr.print("{s}:{d}:{d}: ", .{
            self.fileName,
            line + 1,
            start - startOfLine + 1,
        });
        try self.printError(msg);
        try self.stderr.print("\n{s}\n", .{self.source[startOfLine..i]});
        try self.printIndicator(start - startOfLine, end - startOfLine);
    }
};
