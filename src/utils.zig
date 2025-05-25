const AST = @import("./ast.zig").AST;

pub const Region = struct {
    start: usize,
    end: usize,
};

// Calculate the start and end of the code corresponding to the node of the AST
pub fn computeBoundaries(ast: *AST) Region {
    switch (ast.*) {
        .intConstant => |iC| {
            return .{ .start = iC.token.start, .end = iC.token.end };
        },
        .floatConstant => |fC| {
            return .{ .start = fC.token.start, .end = fC.token.end };
        },
        .boolConstant => |bC| {
            return .{ .start = bC.token.start, .end = bC.token.end };
        },
        .charConstant => |cC| {
            return .{ .start = cC.token.start, .end = cC.token.end };
        },
        .ifExpr => |ifExpr| {
            const end = computeBoundaries(ifExpr.elseExpr).end;
            return .{ .start = ifExpr.start, .end = end };
        },
        .lambda => |lambda| {
            const end = computeBoundaries(lambda.expr).end;
            return .{ .start = lambda.start, .end = end };
        },
        .lambdaMult => |lambdaMult| {
            const end = computeBoundaries(lambdaMult.expr).end;
            return .{ .start = lambdaMult.start, .end = end };
        },
        .call => |call| {
            const end = computeBoundaries(call.arg).end;
            const start = computeBoundaries(call.function).start;
            return .{ .start = start, .end = end };
        },
        .callMult => |callMult| {
            const end = computeBoundaries(callMult.args.items[0]).end;
            const start = computeBoundaries(callMult.function).start;
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
        .prefixOp => |prefixOp| {
            return .{
                .start = prefixOp.token.start,
                .end = computeBoundaries(prefixOp.expr).end,
            };
        },
        .case => |case| {
            const lastBody = case.bodies.items[case.bodies.items.len - 1];
            return .{ .start = case.start, .end = computeBoundaries(lastBody).end };
        },
        .list => |list| {
            return .{ .start = list.start, .end = list.end };
        },
    }
}
