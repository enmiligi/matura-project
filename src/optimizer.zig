const std = @import("std");
const AST = @import("ast.zig").AST;
const token = @import("./token.zig");

pub const OptimizeClosures = struct {
    fn findEnclosed(
        ast: *AST,
        found: *std.ArrayList([]const u8),
        exclude: *std.StringHashMap(void),
    ) !void {
        switch (ast.*) {
            .intConstant, .floatConstant, .boolConstant => {},
            .identifier => |id| {
                if (!exclude.contains(id.token.lexeme)) {
                    try found.append(id.token.lexeme);
                }
            },
            .let => |let| {
                try exclude.put(let.name.lexeme, undefined);
                try findEnclosed(let.in, found, exclude);
                try findEnclosed(let.be, found, exclude);
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
            .lambdaMult => {},
            .callMult => {},
        }
    }

    pub fn run(ast: *AST, allocator: std.mem.Allocator) !void {
        switch (ast.*) {
            .intConstant, .floatConstant, .boolConstant, .identifier => {},
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
            .lambdaMult => {},
            .callMult => {},
        }
    }
};

pub const OptimizeFullyInstantiatedCalls = struct {
    fn combineLambdas(ast: *AST, allocator: std.mem.Allocator) !void {
        errdefer ast.* = .{ .boolConstant = .{
            .token = .{ .type = .BoolLiteral, .start = 0, .end = 0, .lexeme = "" },
            .value = true,
        } };
        var currentExpr = ast;
        var isLambda = true;
        var arguments = std.ArrayList(token.Token).init(allocator);
        while (isLambda) {
            errdefer currentExpr.deinit(allocator);
            try arguments.append(currentExpr.lambda.argname);
            currentExpr = currentExpr.lambda.expr;
            switch (currentExpr.*) {
                .lambda => {
                    isLambda = true;
                },
                else => {
                    isLambda = false;
                },
            }
        }
        const innerExpr = currentExpr;
        currentExpr = ast.lambda.expr;
        isLambda = true;
        while (isLambda) {
            const prev = currentExpr;
            currentExpr = currentExpr.lambda.expr;
            prev.lambda.encloses.?.deinit();
            allocator.destroy(prev);
            switch (currentExpr.*) {
                .lambda => {
                    isLambda = true;
                },
                else => {
                    isLambda = false;
                },
            }
        }
        ast.* = .{ .lambdaMult = .{
            .argnames = arguments,
            .start = ast.lambda.start,
            .expr = innerExpr,
            .encloses = ast.lambda.encloses.?,
        } };
    }

    fn combineCalls(ast: *AST, allocator: std.mem.Allocator) !*AST {
        errdefer ast.* = .{ .boolConstant = .{
            .token = .{ .type = .BoolLiteral, .start = 0, .end = 0, .lexeme = "" },
            .value = true,
        } };
        var arguments: std.ArrayList(*AST) = undefined;
        var fun: *AST = undefined;
        switch (ast.call.function.*) {
            .call => {
                fun = try combineCalls(ast.call.function, allocator);
                arguments = ast.call.function.callMult.args;
                allocator.destroy(ast.call.function);
            },
            else => {
                arguments = .init(allocator);
                fun = ast.call.function;
            },
        }
        try arguments.append(ast.call.arg);
        ast.* = .{ .callMult = .{
            .function = fun,
            .args = arguments,
        } };
        return fun;
    }

    pub fn run(ast: *AST, allocator: std.mem.Allocator) !void {
        switch (ast.*) {
            .intConstant, .floatConstant, .boolConstant, .identifier => {},
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
                        _ = try combineCalls(ast, allocator);
                        try run(ast.callMult.function, allocator);
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
            .lambdaMult => {},
            .callMult => {},
        }
    }
};
