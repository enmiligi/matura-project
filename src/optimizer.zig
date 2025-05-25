const std = @import("std");
const AST = @import("ast.zig").AST;
const Statement = @import("ast.zig").Statement;
const token = @import("./token.zig");

pub const ConvertList = struct {
    pub fn run(ast: *AST, allocator: std.mem.Allocator) !void {
        switch (ast.*) {
            .list => |list| {
                var listExpr = try allocator.create(AST);
                listExpr.* = .{ .identifier = .{ .token = .{
                    .start = list.start,
                    .end = list.end,
                    .lexeme = "Nil",
                    .type = .Identifier,
                } } };
                errdefer listExpr.deinit(allocator);

                for (1..list.values.items.len + 1) |i| {
                    const cons = try allocator.create(AST);
                    cons.* = .{ .identifier = .{ .token = .{
                        .start = list.start,
                        .end = list.end,
                        .lexeme = "Cons",
                        .type = .Identifier,
                    } } };
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

pub const OptimizeClosures = struct {
    fn findEnclosed(
        ast: *AST,
        found: *std.ArrayList([]const u8),
        exclude: *std.StringHashMap(void),
    ) !void {
        switch (ast.*) {
            .intConstant, .floatConstant, .boolConstant, .charConstant => {},
            .identifier => |id| {
                if (!exclude.contains(id.token.lexeme)) {
                    try found.append(id.token.lexeme);
                }
            },
            .let => |let| {
                const alreadyPresent = exclude.contains(let.name.lexeme);
                try exclude.put(let.name.lexeme, undefined);
                try findEnclosed(let.in, found, exclude);
                try findEnclosed(let.be, found, exclude);
                if (!alreadyPresent) {
                    _ = exclude.remove(let.name.lexeme);
                }
            },
            .ifExpr => |ifExpr| {
                try findEnclosed(ifExpr.predicate, found, exclude);
                try findEnclosed(ifExpr.thenExpr, found, exclude);
                try findEnclosed(ifExpr.elseExpr, found, exclude);
            },
            .call => |call| {
                try findEnclosed(call.function, found, exclude);
                try findEnclosed(call.arg, found, exclude);
            },
            .operator => |op| {
                try findEnclosed(op.left, found, exclude);
                try findEnclosed(op.right, found, exclude);
            },
            .prefixOp => |prefixOp| {
                try findEnclosed(prefixOp.expr, found, exclude);
            },
            .lambda => |lambda| {
                const alreadyPresent = exclude.contains(lambda.argname.lexeme);
                try exclude.put(lambda.argname.lexeme, undefined);
                try findEnclosed(lambda.expr, found, exclude);
                if (!alreadyPresent) {
                    _ = exclude.remove(lambda.argname.lexeme);
                }
            },
            .case => |case| {
                try findEnclosed(case.value, found, exclude);
                for (case.patterns.items, case.bodies.items) |pattern, body| {
                    for (pattern.values.items) |value| {
                        try exclude.put(value.lexeme, undefined);
                    }
                    try findEnclosed(body, found, exclude);
                    for (pattern.values.items) |value| {
                        _ = exclude.remove(value.lexeme);
                    }
                }
            },
            .lambdaMult => {},
            .callMult => {},
            .list => {},
        }
    }

    pub fn run(ast: *AST, allocator: std.mem.Allocator) !void {
        switch (ast.*) {
            .intConstant, .floatConstant, .boolConstant, .charConstant, .identifier => {},
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
            .lambda => |*lambda| {
                var found = std.ArrayList([]const u8).init(allocator);
                errdefer found.deinit();
                var exclude = std.StringHashMap(void).init(allocator);
                defer exclude.deinit();
                try exclude.put(lambda.argname.lexeme, undefined);
                try findEnclosed(ast, &found, &exclude);
                try run(lambda.expr, allocator);
                lambda.encloses = found;
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
            .lambdaMult => {},
            .callMult => {},
            .list => {},
        }
    }
};

pub const OptimizeFullyInstantiatedCalls = struct {
    fn combineCalls(ast: *AST, allocator: std.mem.Allocator) !void {
        errdefer ast.* = .{ .boolConstant = .{
            .token = .{ .type = .BoolLiteral, .start = 0, .end = 0, .lexeme = "" },
            .value = true,
        } };
        var currentExpr = ast;
        var isCall = true;
        var arguments = std.ArrayList(*AST).init(allocator);
        while (isCall) {
            errdefer currentExpr.deinit(allocator);
            try arguments.append(currentExpr.call.arg);
            currentExpr = currentExpr.call.function;
            switch (currentExpr.*) {
                .call => {
                    isCall = true;
                },
                else => {
                    isCall = false;
                },
            }
        }
        const innerExpr = currentExpr;
        currentExpr = ast.call.function;
        isCall = true;
        while (isCall) {
            const prev = currentExpr;
            currentExpr = currentExpr.call.function;
            allocator.destroy(prev);
            switch (currentExpr.*) {
                .call => {
                    isCall = true;
                },
                else => {
                    isCall = false;
                },
            }
        }
        ast.* = .{ .callMult = .{
            .args = arguments,
            .function = innerExpr,
        } };
    }

    fn combineLambdas(ast: *AST, allocator: std.mem.Allocator) !void {
        errdefer ast.* = .{ .boolConstant = .{
            .token = .{ .type = .BoolLiteral, .start = 0, .end = 0, .lexeme = "" },
            .value = true,
        } };
        var arguments: std.ArrayList(token.Token) = undefined;
        var expr: *AST = undefined;
        switch (ast.lambda.expr.*) {
            .lambda => {
                try combineLambdas(ast.lambda.expr, allocator);
                arguments = ast.lambda.expr.lambdaMult.argnames;
                expr = ast.lambda.expr.lambdaMult.expr;
                ast.lambda.expr.lambdaMult.encloses.deinit();
                allocator.destroy(ast.lambda.expr);
                if (ast.lambda.argType) |argType| {
                    argType.type.deinit(allocator);
                }
            },
            else => {
                arguments = .init(allocator);
                expr = ast.lambda.expr;
            },
        }
        try arguments.append(ast.lambda.argname);
        const start = ast.lambda.start;
        const encloses = ast.lambda.encloses.?;
        ast.* = .{ .lambdaMult = .{
            .start = start,
            .expr = expr,
            .encloses = encloses,
            .argnames = arguments,
        } };
    }

    pub fn run(ast: *AST, allocator: std.mem.Allocator) !void {
        switch (ast.*) {
            .intConstant, .floatConstant, .boolConstant, .identifier, .charConstant => {},
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
                switch (call.function.*) {
                    .call => {
                        try combineCalls(ast, allocator);
                        try run(ast.callMult.function, allocator);

                        for (ast.callMult.args.items) |arg| {
                            try run(arg, allocator);
                        }
                    },
                    else => {
                        try run(call.function, allocator);
                        try run(call.arg, allocator);
                    },
                }
            },
            .operator => |op| {
                try run(op.left, allocator);
                try run(op.right, allocator);
            },
            .lambda => |lambda| {
                switch (lambda.expr.*) {
                    .lambda => {
                        try combineLambdas(ast, allocator);

                        try run(ast.lambdaMult.expr, allocator);
                    },
                    else => {
                        try run(lambda.expr, allocator);
                    },
                }
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
            .lambdaMult => {},
            .callMult => {},
            .list => {},
        }
    }
};

pub fn optimizeStatement(statement: *Statement, allocator: std.mem.Allocator) !void {
    switch (statement.*) {
        .let => |let| {
            try ConvertList.run(let.be, allocator);
            try OptimizeClosures.run(let.be, allocator);
            try OptimizeFullyInstantiatedCalls.run(let.be, allocator);
        },
        .type => {},
    }
}
