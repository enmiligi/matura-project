const std = @import("std");
const AST = @import("ast.zig").AST;

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
        }
    }
};
