const std = @import("std");
const AST = @import("ast.zig").AST;
const Statement = @import("ast.zig").Statement;
const token = @import("./token.zig");
const Type = @import("./type_inference.zig").Type;

// Create a list of bound variables for each closure
// so not the entire env has to be cloned
pub const GatherBound = struct {
    // Search all referenced variables that have not been declared inside
    fn findEnclosed(
        ast: *AST,
        found: *std.ArrayList([]const u8),
        foundTypes: *std.ArrayList(*Type),
        exclude: *std.StringHashMap(void),
        allocator: std.mem.Allocator,
    ) !void {
        switch (ast.*) {
            .intConstant, .floatConstant, .boolConstant, .charConstant => {},
            .identifier => |id| {
                var name = id.token.lexeme;
                if (id.token.lexeme[0] == '_') {
                    var start: usize = 1;
                    while (std.ascii.isDigit(id.token.lexeme[start]))
                        start += 1;
                    name = id.token.lexeme[start..];
                }
                if (!exclude.contains(name)) {
                    try found.append(allocator, id.token.lexeme);
                    id.idType.?.rc += 1;
                    try foundTypes.append(allocator, id.idType.?);
                }
            },
            .let => |let| {
                try exclude.put(let.name.lexeme, undefined);
                if (let.monomorphizations) |monomorphizations| {
                    for (monomorphizations.items) |monomorphization| {
                        try findEnclosed(monomorphization, found, foundTypes, exclude, allocator);
                    }
                } else {
                    try findEnclosed(let.be, found, foundTypes, exclude, allocator);
                }
                try findEnclosed(let.in, found, foundTypes, exclude, allocator);
            },
            .ifExpr => |ifExpr| {
                try findEnclosed(ifExpr.predicate, found, foundTypes, exclude, allocator);
                try findEnclosed(ifExpr.thenExpr, found, foundTypes, exclude, allocator);
                try findEnclosed(ifExpr.elseExpr, found, foundTypes, exclude, allocator);
            },
            .call => |call| {
                try findEnclosed(call.function, found, foundTypes, exclude, allocator);
                try findEnclosed(call.arg, found, foundTypes, exclude, allocator);
            },
            .operator => |op| {
                try findEnclosed(op.left, found, foundTypes, exclude, allocator);
                try findEnclosed(op.right, found, foundTypes, exclude, allocator);
            },
            .prefixOp => |prefixOp| {
                try findEnclosed(
                    prefixOp.expr,
                    found,
                    foundTypes,
                    exclude,
                    allocator,
                );
            },
            .lambda => |lambda| {
                const alreadyPresent = exclude.contains(lambda.argname.lexeme);
                try exclude.put(lambda.argname.lexeme, undefined);
                try findEnclosed(
                    lambda.expr,
                    found,
                    foundTypes,
                    exclude,
                    allocator,
                );
                if (!alreadyPresent) {
                    _ = exclude.remove(lambda.argname.lexeme);
                }
            },
            .case => |case| {
                try findEnclosed(
                    case.value,
                    found,
                    foundTypes,
                    exclude,
                    allocator,
                );
                for (case.patterns.items, case.bodies.items) |pattern, body| {
                    for (pattern.values.items) |value| {
                        try exclude.put(value.lexeme, undefined);
                    }
                    try findEnclosed(
                        body,
                        found,
                        foundTypes,
                        exclude,
                        allocator,
                    );
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

    // Run the optimization on each contained sub-AST
    pub fn run(ast: *AST, allocator: std.mem.Allocator) !void {
        switch (ast.*) {
            .intConstant, .floatConstant, .boolConstant, .charConstant, .identifier => {},
            .let => |let| {
                if (let.monomorphizations) |monomorphizations| {
                    for (monomorphizations.items) |monomorphization| {
                        try run(monomorphization, allocator);
                    }
                } else {
                    try run(let.be, allocator);
                }
                try run(let.in, allocator);
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
                var found = std.ArrayList([]const u8).empty;
                errdefer found.deinit(allocator);
                var foundTypes = std.ArrayList(*Type).empty;
                errdefer foundTypes.deinit(allocator);
                var exclude = std.StringHashMap(void).init(allocator);
                defer exclude.deinit();
                try exclude.put(lambda.argname.lexeme, undefined);
                try findEnclosed(
                    ast,
                    &found,
                    &foundTypes,
                    &exclude,
                    allocator,
                );
                lambda.encloses = found;
                lambda.enclosesTypes = foundTypes;
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
            .lambdaMult => {},
            .callMult => {},
            .list => {},
        }
    }
};

// Convert consecutive lambdas or calls into one object
pub const CombineCallsAndLambdas = struct {
    fn combineCalls(ast: *AST, allocator: std.mem.Allocator) !void {
        errdefer ast.* = .{ .boolConstant = .{
            .token = .{ .type = .BoolLiteral, .start = 0, .end = 0, .lexeme = "" },
            .value = true,
        } };
        var currentExpr = ast;
        var isCall = true;
        var arguments = std.ArrayList(*AST).empty;
        while (isCall) {
            errdefer currentExpr.deinit(allocator);
            try arguments.append(allocator, currentExpr.call.arg);
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
            if (prev.call.functionType) |t| {
                t.deinit(allocator);
            }
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
        if (ast.call.functionType) |t| {
            t.deinit(allocator);
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
                ast.lambda.expr.lambdaMult.encloses.deinit(allocator);
                allocator.destroy(ast.lambda.expr);
            },
            else => {
                arguments = .empty;
                expr = ast.lambda.expr;
            },
        }
        if (ast.lambda.argType) |argType| {
            argType.type.deinit(allocator);
        }
        if (ast.lambda.enclosesTypes) |*enclosesTypes| {
            for (enclosesTypes.items) |t| {
                t.deinit(allocator);
            }
            enclosesTypes.deinit(allocator);
        }
        if (ast.lambda.type) |t| {
            t.deinit(allocator);
        }
        try arguments.append(allocator, ast.lambda.argname);
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

// Run all optimizations consecutively on a statement
pub fn optimizeStatement(statement: *Statement, allocator: std.mem.Allocator, interpreted: bool) !void {
    switch (statement.*) {
        .let => |let| {
            if (let.monomorphizations) |monomorphizations| {
                for (monomorphizations.items) |monomorphization| {
                    try GatherBound.run(monomorphization, allocator);
                    if (interpreted)
                        try CombineCallsAndLambdas.run(monomorphization, allocator);
                }
            } else {
                try GatherBound.run(let.be, allocator);
                if (interpreted)
                    try CombineCallsAndLambdas.run(let.be, allocator);
            }
        },
        .type => {},
    }
}
