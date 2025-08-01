const std = @import("std");
const AST = @import("ast.zig").AST;
const Statement = @import("ast.zig").Statement;

// Convert all lists to consecutive calls of Cons
pub const ConvertList = struct {
    pub fn run(ast: *AST, allocator: std.mem.Allocator) !void {
        switch (ast.*) {
            .list => |list| {
                var listExpr = try allocator.create(AST);
                listExpr.* = .{ .identifier = .{
                    .token = .{
                        .start = list.start,
                        .end = list.end,
                        .lexeme = "Nil",
                        .type = .Identifier,
                    },
                    .idType = list.nilType,
                } };
                errdefer listExpr.deinit(allocator);

                for (list.values.items) |value| {
                    try run(value, allocator);
                }

                for (1..list.values.items.len + 1) |i| {
                    const cons = try allocator.create(AST);
                    list.consType.?.rc += 1;
                    cons.* = .{ .identifier = .{
                        .token = .{
                            .start = list.start,
                            .end = list.end,
                            .lexeme = "Cons",
                            .type = .Identifier,
                        },
                        .idType = list.consType,
                    } };
                    errdefer cons.deinit(allocator);
                    const firstCall = try allocator.create(AST);
                    firstCall.* = .{ .call = .{
                        .function = cons,
                        .arg = list.values.items[list.values.items.len - i],
                    } };
                    errdefer firstCall.deinit(allocator);
                    const secondCall = try allocator.create(AST);
                    secondCall.* = .{ .call = .{
                        .function = firstCall,
                        .arg = listExpr,
                    } };
                    listExpr = secondCall;
                }
                list.values.deinit();
                list.consType.?.deinit(allocator);
                ast.* = listExpr.*;
                allocator.destroy(listExpr);
            },
            .let => |let| {
                try run(let.in, allocator);
                try run(let.be, allocator);
            },
            .ifExpr => |ifExpr| {
                try run(ifExpr.predicate, allocator);
                try run(ifExpr.thenExpr, allocator);
                try run(ifExpr.elseExpr, allocator);
            },
            .call => |call| {
                try run(call.function, allocator);
                try run(call.arg, allocator);
            },
            .operator => |op| {
                try run(op.left, allocator);
                try run(op.right, allocator);
            },
            .lambda => |lambda| {
                try run(lambda.expr, allocator);
            },
            .prefixOp => |prefixOp| {
                try run(prefixOp.expr, allocator);
            },
            .case => |case| {
                try run(case.value, allocator);
                for (case.bodies.items) |body| {
                    try run(body, allocator);
                }
            },
            .lambdaMult,
            .callMult,
            .intConstant,
            .floatConstant,
            .boolConstant,
            .charConstant,
            .identifier,
            => {},
        }
    }
};

// Desugar a statement
pub fn desugarStatement(statement: *Statement, allocator: std.mem.Allocator) !void {
    switch (statement.*) {
        .let => |let| {
            if (let.monomorphizations) |monomorphizations| {
                for (monomorphizations.items) |monomorphization| {
                    try ConvertList.run(monomorphization, allocator);
                }
            } else {
                try ConvertList.run(let.be, allocator);
            }
        },
        .type => {},
    }
}
