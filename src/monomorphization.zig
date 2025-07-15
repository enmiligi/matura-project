const std = @import("std");
const type_inference = @import("type_inference.zig");
const Type = type_inference.Type;
const TypeScheme = type_inference.TypeScheme;
const AST = @import("ast.zig").AST;

pub const Monomorphizer = struct {
    pub fn instantiateFreeVars(t: *Type, boundVars: *std.AutoHashMap(usize, void)) void {
        switch (t.data) {
            .typeVar => |tV| {
                if (tV.subst) |subst| {
                    instantiateFreeVars(subst, boundVars);
                } else {
                    if (!boundVars.contains(tV.n)) {
                        t.data = .{ .primitive = .Int };
                    }
                }
            },
            .function => |function| {
                instantiateFreeVars(function.from, boundVars);
                instantiateFreeVars(function.to, boundVars);
            },
            .composite => |composite| {
                for (composite.args.items) |compositeType| {
                    instantiateFreeVars(compositeType, boundVars);
                }
            },
            .number => |number| {
                instantiateFreeVars(number.variable, boundVars);
            },
            .primitive => {},
        }
    }

    pub fn instantiateAST(ast: *AST, boundVars: *std.AutoHashMap(usize, void)) !void {
        switch (ast.*) {
            .let => |let| {
                var variableType: *Type = undefined;
                switch (let.actualType.?.*) {
                    .forall => |forall| {
                        for (forall.typeVars.keys()) |tVNum| {
                            try boundVars.put(tVNum, undefined);
                        }
                        variableType = forall.type;
                    },
                    .type => |t| {
                        variableType = t;
                    },
                }
                instantiateFreeVars(variableType, boundVars);
                try instantiateAST(let.be, boundVars);
                switch (let.actualType.?.*) {
                    .forall => |forall| {
                        for (forall.typeVars.keys()) |tVNum| {
                            _ = boundVars.remove(tVNum);
                        }
                    },
                    .type => {},
                }
                try instantiateAST(let.in, boundVars);
            },
            .lambda => |lambda| {
                instantiateFreeVars(lambda.type.?, boundVars);
                for (lambda.enclosesTypes.?.items) |enclosedType| {
                    instantiateFreeVars(enclosedType, boundVars);
                }
                try instantiateAST(lambda.expr, boundVars);
            },
            .lambdaMult => {},
            .call => |call| {
                instantiateFreeVars(call.functionType.?, boundVars);
                try instantiateAST(call.function, boundVars);
                try instantiateAST(call.arg, boundVars);
            },
            .callMult => {},
            .ifExpr => |ifExpr| {
                instantiateFreeVars(ifExpr.resultType.?, boundVars);
                try instantiateAST(ifExpr.predicate, boundVars);
                try instantiateAST(ifExpr.thenExpr, boundVars);
                try instantiateAST(ifExpr.elseExpr, boundVars);
            },
            .intConstant, .floatConstant, .boolConstant, .charConstant => {},
            .identifier => |id| {
                instantiateFreeVars(id.idType.?, boundVars);
            },
            .operator => |op| {
                instantiateFreeVars(op.argType.?, boundVars);
                try instantiateAST(op.left, boundVars);
                try instantiateAST(op.right, boundVars);
            },
            .prefixOp => |op| {
                instantiateFreeVars(op.argType.?, boundVars);
                try instantiateAST(op.expr, boundVars);
            },
            // TODO
            .case => {},
            .list => {},
        }
    }
};
