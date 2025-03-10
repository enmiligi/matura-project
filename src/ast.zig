const std = @import("std");
const token = @import("./token.zig");

pub const AST = union(enum) {
    let: struct {
        start: usize,
        name: token.Token,
        be: *AST,
        in: *AST,
    },
    lambda: struct {
        start: usize,
        argname: token.Token,
        expr: *AST,
    },
    call: struct {
        function: *AST,
        arg: *AST,
    },
    intConstant: struct {
        token: token.Token,
        value: i64,
    },
    floatConstant: struct {
        token: token.Token,
        value: f64,
    },
    boolConstant: struct {
        token: token.Token,
        value: bool,
    },
    identifier: struct {
        token: token.Token,
    },
    operator: struct {
        token: token.Token,
        left: *AST,
        right: *AST,
    },

    // Recursively destroy all contained ASTs
    pub fn deinit(self: *AST, allocator: std.mem.Allocator) void {
        switch (self.*) {
            .let => |let| {
                let.be.deinit(allocator);
                let.in.deinit(allocator);
            },
            .lambda => |lambda| {
                lambda.expr.deinit(allocator);
            },
            .call => |call| {
                call.function.deinit(allocator);
                call.arg.deinit(allocator);
            },
            .operator => |op| {
                op.left.deinit(allocator);
                op.right.deinit(allocator);
            },
            else => {},
        }
        allocator.destroy(self);
    }

    pub fn print(self: AST, writer: std.io.AnyWriter) !void {
        switch (self) {
            .let => |let| {
                try writer.print("Let(name: {s}, be: ", .{let.name.lexeme});
                try let.be.print(writer);
                try writer.print(", in: ", .{});
                try let.in.print(writer);
                try writer.print(")", .{});
            },
            .lambda => |lambda| {
                try writer.print("Lambda(argname: {s}, expr: ", .{lambda.argname.lexeme});
                try lambda.expr.print(writer);
                try writer.print(")", .{});
            },
            .call => |call| {
                try writer.print("Call(function: ", .{});
                try call.function.print(writer);
                try writer.print(", arg: ", .{});
                try call.arg.print(writer);
                try writer.print(")", .{});
            },
            .intConstant => |int| {
                try writer.print("Int({s})", .{int.token.lexeme});
            },
            .floatConstant => |float| {
                try writer.print("Float({s})", .{float.token.lexeme});
            },
            .boolConstant => |boolConst| {
                try writer.print("Bool({s})", .{boolConst.token.lexeme});
            },
            .identifier => |id| {
                try writer.print("Identifier({s})", .{id.token.lexeme});
            },
            .operator => |op| {
                try writer.print("(", .{});
                try op.left.print(writer);
                try writer.print("{s}", .{op.token.lexeme});
                try op.right.print(writer);
                try writer.print(")", .{});
            },
        }
    }
};
