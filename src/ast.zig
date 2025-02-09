const std = @import("std");
const token = @import("./token.zig");

pub const AST = union(enum) {
    let: struct {
        name: token.Token,
        be: *AST,
        in: *AST,
    },
    lambda: struct {
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
    identifier: struct {
        token: token.Token,
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
            else => {},
        }
        allocator.destroy(self);
    }

    // Only meant to be used for debugging purposes
    // since not using stdout but debug.print
    pub fn debugPrint(self: AST) void {
        switch (self) {
            .let => |let| {
                std.debug.print("Let(name: {s}, to: ", .{let.name.lexeme});
                let.be.debugPrint();
                std.debug.print(", in: ", .{});
                let.in.debugPrint();
                std.debug.print(")", .{});
            },
            .lambda => |lambda| {
                std.debug.print("Lambda(argname: {s}, expr: ", .{lambda.argname.lexeme});
                lambda.expr.debugPrint();
                std.debug.print(")", .{});
            },
            .call => |call| {
                std.debug.print("Call(function: ", .{});
                call.function.debugPrint();
                std.debug.print(", arg: ", .{});
                call.arg.debugPrint();
                std.debug.print(")", .{});
            },
            .intConstant => |int| {
                std.debug.print("Int({s})", .{int.token.lexeme});
            },
            .floatConstant => |float| {
                std.debug.print("Float({s})", .{float.token.lexeme});
            },
            .identifier => |id| {
                std.debug.print("Identifier({s})", .{id.token.lexeme});
            },
        }
    }
};
