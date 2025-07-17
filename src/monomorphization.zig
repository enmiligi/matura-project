const std = @import("std");
const type_inference = @import("type_inference.zig");
const Type = type_inference.Type;
const TypeScheme = type_inference.TypeScheme;
const Forall = type_inference.Forall;
const AST = @import("ast.zig").AST;

const MultiSubstitution = struct {
    variables: []usize,
    instantiations: *std.ArrayList([]*Type),
};

pub const Monomorphizer = struct {
    allocator: std.mem.Allocator,
    idToTypeScheme: std.StringHashMap(*TypeScheme),
    idToInstantiations: std.StringHashMap(*std.ArrayList([]*Type)),
    algorithmJ: *type_inference.AlgorithmJ,
    monomorphizeDefinition: ?[]const u8 = null,
    monomorphizeNumber: ?usize = null,

    pub fn init(
        allocator: std.mem.Allocator,
        algorithmJ: *type_inference.AlgorithmJ,
    ) Monomorphizer {
        return .{
            .allocator = allocator,
            .idToTypeScheme = .init(allocator),
            .idToInstantiations = .init(allocator),
            .algorithmJ = algorithmJ,
        };
    }

    pub fn deinit(self: *Monomorphizer) void {
        self.idToInstantiations.deinit();
        self.idToTypeScheme.deinit();
    }

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

    fn getInstantiation(
        self: *Monomorphizer,
        forallVars: std.AutoArrayHashMap(usize, void),
        forallType: *Type,
        t: *Type,
        setInstantiations: []*Type,
    ) void {
        switch (forallType.data) {
            .typeVar => |tV| {
                if (tV.subst) |subst| {
                    self.getInstantiation(
                        forallVars,
                        subst,
                        t,
                        setInstantiations,
                    );
                } else {
                    if (forallVars.contains(tV.n)) {
                        setInstantiations[forallVars.getIndex(tV.n).?] = t;
                    }
                }
            },
            .number => |number| {
                self.getInstantiation(
                    forallVars,
                    number.variable,
                    t,
                    setInstantiations,
                );
            },
            .function => |function| {
                self.getInstantiation(
                    forallVars,
                    function.from,
                    t.data.function.from,
                    setInstantiations,
                );
                self.getInstantiation(
                    forallVars,
                    function.to,
                    t.data.function.to,
                    setInstantiations,
                );
            },
            .composite => |composite| {
                for (composite.args.items, 0..) |arg, i| {
                    self.getInstantiation(
                        forallVars,
                        arg,
                        composite.args.items[i],
                        setInstantiations,
                    );
                }
            },
            .primitive => {},
        }
    }

    pub fn monomorphize(self: *Monomorphizer, expr: *AST) !void {
        switch (expr.*) {
            .intConstant, .floatConstant, .boolConstant, .charConstant => {},
            .identifier => |*id| {
                // Could be the argument to a function
                if (self.idToTypeScheme.contains(id.token.lexeme)) {
                    switch (self.idToTypeScheme.get(id.token.lexeme).?.*) {
                        .forall => |forall| {
                            const types = try self.allocator.alloc(*Type, forall.typeVars.count());
                            errdefer self.allocator.free(types);
                            self.getInstantiation(forall.typeVars, forall.type, id.idType.?, types);
                            const instantiations = self.idToInstantiations.get(id.token.lexeme).?;
                            var present = false;
                            var index = instantiations.items.len;
                            outer: for (instantiations.items, 0..) |instantiation, i| {
                                for (types, instantiation) |t1, t2| {
                                    if (!typesEqual(t1, t2)) {
                                        continue :outer;
                                    }
                                }
                                present = true;
                                index = i;
                            }
                            if (!present) {
                                try instantiations.append(types);
                            }
                            id.token.lexeme = try std.fmt.allocPrint(
                                self.allocator,
                                "_{d}{s}",
                                .{ index, id.token.lexeme },
                            );
                        },
                        else => {},
                    }
                }
            },
            .call => |call| {
                try self.monomorphize(call.arg);
                try self.monomorphize(call.function);
            },
            .lambda => |lambda| {
                try self.monomorphize(lambda.expr);
            },
            .lambdaMult, .callMult => {},
            .let => |*let| {
                let.instantiations = .init(self.allocator);
                try self.idToInstantiations.put(let.name.lexeme, &let.instantiations.?);
                try self.idToTypeScheme.put(let.name.lexeme, let.actualType.?);
                try self.monomorphize(let.in);
                _ = self.idToInstantiations.remove(let.name.lexeme);
                _ = self.idToTypeScheme.remove(let.name.lexeme);
                switch (let.actualType.?.*) {
                    .type => {
                        try self.monomorphize(let.be);
                    },
                    .forall => |forall| {
                        var monomorphizations = try std.ArrayList(*AST).initCapacity(
                            self.allocator,
                            let.instantiations.?.items.len,
                        );
                        errdefer for (monomorphizations.items) |monomorphization| {
                            monomorphization.deinit(self.allocator);
                        };
                        errdefer monomorphizations.deinit();
                        for (let.instantiations.?.items, 0..) |instantiation, i| {
                            self.monomorphizeDefinition = let.name.lexeme;
                            self.monomorphizeNumber = i;
                            try monomorphizations.append(try self.monomorphizeAST(
                                let.be,
                                forall.typeVars.keys(),
                                instantiation,
                            ));
                        }
                        let.monomorphizations = monomorphizations;
                        for (monomorphizations.items) |monomorphization| {
                            try self.monomorphize(monomorphization);
                        }
                    },
                }
            },
            .operator => |op| {
                try self.monomorphize(op.right);
                try self.monomorphize(op.left);
            },
            .prefixOp => |op| {
                try self.monomorphize(op.expr);
            },
            .ifExpr => |ifExpr| {
                try self.monomorphize(ifExpr.elseExpr);
                try self.monomorphize(ifExpr.thenExpr);
                try self.monomorphize(ifExpr.predicate);
            },
            // TODO
            .case, .list => {},
        }
    }

    fn monomorphizeAST(self: *Monomorphizer, ast: *AST, variables: []usize, groundTypes: []*Type) !*AST {
        const copiedAST = try self.allocator.create(AST);
        errdefer self.allocator.destroy(copiedAST);
        switch (ast.*) {
            .intConstant, .boolConstant, .floatConstant, .charConstant => {
                copiedAST.* = ast.*;
                return copiedAST;
            },
            .identifier => |id| {
                const copiedType = try self.monomorphizeType(id.idType.?, variables, groundTypes);
                errdefer copiedType.deinit(self.allocator);
                var token = id.token;
                if (std.mem.eql(u8, id.token.lexeme, self.monomorphizeDefinition.?)) {
                    token.lexeme = try std.fmt.allocPrint(
                        self.allocator,
                        "_{d}{s}",
                        .{
                            self.monomorphizeNumber.?,
                            id.token.lexeme,
                        },
                    );
                }
                copiedAST.* = .{ .identifier = .{
                    .token = token,
                    .idType = copiedType,
                } };
            },
            .call => |call| {
                const copiedFunction = try self.monomorphizeAST(
                    call.function,
                    variables,
                    groundTypes,
                );
                errdefer copiedFunction.deinit(self.allocator);
                const copiedArgument = try self.monomorphizeAST(
                    call.arg,
                    variables,
                    groundTypes,
                );
                errdefer copiedArgument.deinit(self.allocator);
                const copiedType = try self.monomorphizeType(
                    call.functionType.?,
                    variables,
                    groundTypes,
                );
                copiedAST.* = .{ .call = .{
                    .function = copiedFunction,
                    .arg = copiedArgument,
                    .functionType = copiedType,
                } };
            },
            .lambda => |lambda| {
                const copiedExpr = try self.monomorphizeAST(
                    lambda.expr,
                    variables,
                    groundTypes,
                );
                errdefer copiedExpr.deinit(self.allocator);
                const copiedType = try self.monomorphizeType(
                    lambda.type.?,
                    variables,
                    groundTypes,
                );
                copiedAST.* = .{ .lambda = .{
                    .start = lambda.start,
                    .argname = lambda.argname,
                    .argType = null,
                    .expr = copiedExpr,
                    .type = copiedType,
                } };
            },
            .lambdaMult, .callMult => {},
            .let => |let| {
                const copiedValue = try self.monomorphizeAST(
                    let.be,
                    variables,
                    groundTypes,
                );
                errdefer copiedValue.deinit(self.allocator);
                const copiedExpr = try self.monomorphizeAST(
                    let.in,
                    variables,
                    groundTypes,
                );
                errdefer copiedExpr.deinit(self.allocator);
                const copiedType: *TypeScheme = try self.allocator.create(TypeScheme);
                errdefer self.allocator.destroy(copiedType);
                switch (let.actualType.?.*) {
                    .type => |t| {
                        copiedType.* = .{ .type = try self.monomorphizeType(
                            t,
                            variables,
                            groundTypes,
                        ) };
                    },
                    .forall => |forall| {
                        var copiedVars = try forall.typeVars.clone();
                        errdefer copiedVars.deinit();
                        copiedType.* = .{ .forall = .{
                            .typeVars = copiedVars,
                            .type = try self.monomorphizeType(forall.type, variables, groundTypes),
                        } };
                    },
                }
                copiedAST.* = .{ .let = .{
                    .start = let.start,
                    .name = let.name,
                    .be = copiedValue,
                    .type = null,
                    .actualType = copiedType,
                    .instantiations = null,
                    .in = copiedExpr,
                } };
            },
            .ifExpr => |ifExpr| {
                const copiedPredicate = try self.monomorphizeAST(
                    ifExpr.predicate,
                    variables,
                    groundTypes,
                );
                errdefer copiedPredicate.deinit(self.allocator);
                const copiedThen = try self.monomorphizeAST(
                    ifExpr.thenExpr,
                    variables,
                    groundTypes,
                );
                errdefer copiedThen.deinit(self.allocator);
                const copiedElse = try self.monomorphizeAST(
                    ifExpr.elseExpr,
                    variables,
                    groundTypes,
                );
                errdefer copiedElse.deinit(self.allocator);
                const copiedType = try self.monomorphizeType(
                    ifExpr.resultType.?,
                    variables,
                    groundTypes,
                );
                copiedAST.* = .{ .ifExpr = .{
                    .start = ifExpr.start,
                    .predicate = copiedPredicate,
                    .thenExpr = copiedThen,
                    .elseExpr = copiedElse,
                    .resultType = copiedType,
                } };
            },
            .operator => |op| {
                const copiedLeft = try self.monomorphizeAST(
                    op.left,
                    variables,
                    groundTypes,
                );
                errdefer copiedLeft.deinit(self.allocator);
                const copiedRight = try self.monomorphizeAST(
                    op.right,
                    variables,
                    groundTypes,
                );
                errdefer copiedRight.deinit(self.allocator);
                const copiedType = try self.monomorphizeType(
                    op.argType.?,
                    variables,
                    groundTypes,
                );
                copiedAST.* = .{ .operator = .{
                    .token = op.token,
                    .left = copiedLeft,
                    .right = copiedRight,
                    .argType = copiedType,
                } };
            },
            .prefixOp => |op| {
                const copiedRight = try self.monomorphizeAST(
                    op.expr,
                    variables,
                    groundTypes,
                );
                errdefer copiedRight.deinit(self.allocator);
                const copiedType = try self.monomorphizeType(
                    op.argType.?,
                    variables,
                    groundTypes,
                );
                copiedAST.* = .{ .prefixOp = .{
                    .token = op.token,
                    .expr = copiedRight,
                    .argType = copiedType,
                } };
            },
            // TODO
            .case, .list => {},
        }
        return copiedAST;
    }

    fn monomorphizeType(self: *Monomorphizer, t: *Type, variables: []usize, groundTypes: []*Type) !*Type {
        switch (t.data) {
            .typeVar => |typeVar| {
                if (typeVar.subst) |subst| {
                    const copiedSubst = try self.monomorphizeType(subst, variables, groundTypes);
                    errdefer copiedSubst.deinit(self.allocator);
                    const typeVarCopied = try Type.init(self.allocator);
                    typeVarCopied.* = self.algorithmJ.newVarT();
                    typeVarCopied.data.typeVar.subst = copiedSubst;
                    return typeVarCopied;
                }
                for (variables, 0..) |variable, i| {
                    if (variable == typeVar.n) {
                        const typeVarCopied = try self.allocator.create(Type);
                        typeVarCopied.* = self.algorithmJ.newVarT();
                        typeVarCopied.data.typeVar.subst = groundTypes[i];
                        groundTypes[i].rc += 1;
                        return typeVarCopied;
                    }
                }
                t.rc += 1;
                return t;
            },
            .function => |function| {
                const copiedFrom = try self.monomorphizeType(function.from, variables, groundTypes);
                errdefer copiedFrom.deinit(self.allocator);
                const copiedTo = try self.monomorphizeType(function.to, variables, groundTypes);
                errdefer copiedTo.deinit(self.allocator);
                if (function.from == copiedFrom and function.to == copiedTo) {
                    copiedFrom.rc -= 1;
                    copiedTo.rc -= 1;
                    t.rc += 1;
                    return t;
                }
                const copiedFunction = try Type.init(self.allocator);
                copiedFunction.data = .{ .function = .{
                    .from = copiedFrom,
                    .to = copiedTo,
                } };
                return copiedFunction;
            },
            .number => |number| {
                const copiedTypeVar = try self.monomorphizeType(number.variable, variables, groundTypes);
                errdefer copiedTypeVar.deinit(self.allocator);
                if (number.variable == copiedTypeVar) {
                    copiedTypeVar.rc -= 1;
                    t.rc += 1;
                    return t;
                }
                const copiedNumber = try Type.init(self.allocator);
                copiedNumber.data = .{ .number = .{
                    .variable = copiedTypeVar,
                } };
                return copiedNumber;
            },
            .composite => |composite| {
                var copiedArgs = std.ArrayList(*Type).init(self.allocator);
                errdefer for (copiedArgs.items) |copied| {
                    copied.deinit(self.allocator);
                };
                errdefer copiedArgs.deinit();
                var notCopied = true;
                for (composite.args.items) |arg| {
                    const copiedArg = try self.monomorphizeType(
                        arg,
                        variables,
                        groundTypes,
                    );
                    if (copiedArg != arg) {
                        notCopied = false;
                    }
                    try copiedArgs.append(copiedArg);
                }
                if (notCopied) {
                    for (copiedArgs.items) |arg| {
                        arg.rc -= 1;
                    }
                    copiedArgs.deinit();
                    t.rc += 1;
                    return t;
                }
                const copiedComposite = try Type.init(self.allocator);
                copiedComposite.data = .{ .composite = .{
                    .name = composite.name,
                    .args = copiedArgs,
                } };
                return copiedComposite;
            },
            .primitive => {
                t.rc += 1;
                return t;
            },
        }
    }

    fn monomorphizeTypes(
        self: *Monomorphizer,
        types: []*Type,
        variables: []usize,
        groundTypes: []*Type,
    ) []*Type {
        const copiedTypes = try self.allocator.alloc(*Type, types.len);
        var i = 0;
        errdefer for (0..i) |j| {
            copiedTypes[j].deinit(self.allocator);
        };
        while (i < types.len) : (i += 1) {
            copiedTypes[i] = try self.monomorphizeType(
                types[i],
                variables,
                groundTypes,
            );
        }
        return copiedTypes;
    }

    fn typesEqual(left: *Type, right: *Type) bool {
        switch (right.data) {
            .typeVar => |tV1| {
                if (tV1.subst) |subst| {
                    return typesEqual(left, subst);
                }
            },
            else => {},
        }
        switch (left.data) {
            .typeVar => |tV1| {
                if (tV1.subst) |subst| {
                    return typesEqual(subst, right);
                }
                switch (right.data) {
                    .typeVar => |tV2| {
                        return tV1.n == tV2.n;
                    },
                    else => {
                        return false;
                    },
                }
            },
            .primitive => |primitive1| {
                switch (right.data) {
                    .primitive => |primitive2| {
                        return primitive1 == primitive2;
                    },
                    .number => |number| {
                        return typesEqual(left, number.variable);
                    },
                    else => {
                        return false;
                    },
                }
            },
            .function => |function1| {
                switch (right.data) {
                    .function => |function2| {
                        return typesEqual(function1.from, function2.from) and typesEqual(function1.to, function2.to);
                    },
                    else => {
                        return false;
                    },
                }
            },
            .composite => |composite1| {
                switch (right.data) {
                    .composite => |composite2| {
                        if (!std.mem.eql(u8, composite1.name, composite2.name)) {
                            return false;
                        }
                        for (composite1.args.items, composite2.args.items) |type1, type2| {
                            if (!typesEqual(type1, type2)) {
                                return false;
                            }
                        }
                        return true;
                    },
                    else => {
                        return false;
                    },
                }
            },
            .number => |number1| {
                switch (right.data) {
                    .number => |number2| {
                        return typesEqual(number1.variable, number2.variable);
                    },
                    else => {
                        return false;
                    },
                }
            },
        }
    }
};
