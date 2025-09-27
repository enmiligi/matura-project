const std = @import("std");
const type_inference = @import("type_inference.zig");
const Type = type_inference.Type;
const TypeScheme = type_inference.TypeScheme;
const Forall = type_inference.Forall;
const AST = @import("ast.zig").AST;
const Statement = @import("ast.zig").Statement;
const Pattern = @import("ast.zig").Pattern;

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
    charType: *Type,
    intType: *Type,
    floatType: *Type,

    pub fn init(
        allocator: std.mem.Allocator,
        algorithmJ: *type_inference.AlgorithmJ,
    ) !Monomorphizer {
        const charType = try Type.init(allocator);
        charType.data = .{ .primitive = .Char };
        errdefer charType.deinit(allocator);
        const intType = try Type.init(allocator);
        intType.data = .{ .primitive = .Int };
        errdefer intType.deinit(allocator);
        const floatType = try Type.init(allocator);
        floatType.data = .{ .primitive = .Float };
        return .{
            .allocator = allocator,
            .idToTypeScheme = .init(allocator),
            .idToInstantiations = .init(allocator),
            .algorithmJ = algorithmJ,
            .charType = charType,
            .intType = intType,
            .floatType = floatType,
        };
    }

    pub fn deinit(self: *Monomorphizer) void {
        self.idToInstantiations.deinit();
        self.idToTypeScheme.deinit();
        self.charType.deinit(self.allocator);
        self.floatType.deinit(self.allocator);
        self.intType.deinit(self.allocator);
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

    pub fn instantiate(
        allocator: std.mem.Allocator,
        statements: *std.ArrayList(Statement),
    ) !void {
        var boundVars = std.AutoHashMap(usize, void).init(allocator);
        defer boundVars.deinit();
        for (statements.items) |*statement| {
            switch (statement.*) {
                .let => |*let| {
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
                    instantiateFreeVars(variableType, &boundVars);
                    try instantiateAST(let.be, &boundVars);
                    switch (let.actualType.?.*) {
                        .forall => |forall| {
                            for (forall.typeVars.keys()) |tVNum| {
                                _ = boundVars.remove(tVNum);
                            }
                        },
                        .type => {},
                    }
                },
                .type => {},
            }
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
            .case => |case| {
                instantiateFreeVars(case.valueType.?, boundVars);
                try instantiateAST(case.value, boundVars);
                for (case.bodies.items) |body| {
                    try instantiateAST(body, boundVars);
                }
            },
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
                        t.data.typeVar.subst.?,
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
                    t.data.number.variable,
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
                for (composite.args.items, t.data.composite.args.items) |arg1, arg2| {
                    self.getInstantiation(
                        forallVars,
                        arg1,
                        arg2,
                        setInstantiations,
                    );
                }
            },
            .primitive => {},
        }
    }

    fn monomorphizeDefinitions(
        self: *Monomorphizer,
        typeScheme: *TypeScheme,
        name: []const u8,
        expr: *AST,
        instantiations: *std.ArrayList([]*Type),
        monomorphizationsLocation: *?std.ArrayList(*AST),
    ) !void {
        switch (typeScheme.*) {
            .type => {
                try self.monomorphizeExpr(expr);
            },
            .forall => |forall| {
                var monomorphizations = try std.ArrayList(*AST).initCapacity(
                    self.allocator,
                    instantiations.items.len,
                );
                errdefer for (monomorphizations.items) |monomorphization| {
                    monomorphization.deinit(self.allocator);
                };
                errdefer monomorphizations.deinit(self.allocator);
                for (instantiations.items, 0..) |instantiation, i| {
                    self.monomorphizeDefinition = name;
                    self.monomorphizeNumber = i;
                    try monomorphizations.append(self.allocator, try self.monomorphizeAST(
                        expr,
                        forall.typeVars.keys(),
                        instantiation,
                    ));
                }
                for (monomorphizations.items) |monomorphization| {
                    try self.monomorphizeExpr(monomorphization);
                }
                monomorphizationsLocation.* = monomorphizations;
            },
        }
    }

    // Monomorphizes a file and returns the name of the correct main function
    pub fn monomorphize(
        self: *Monomorphizer,
        file: *std.ArrayList(Statement),
        mainType: *Type,
    ) ![]const u8 {
        for (file.items) |*statement| {
            switch (statement.*) {
                .let => |*let| {
                    let.instantiations = .empty;
                    try self.idToInstantiations.put(let.name.lexeme, &let.instantiations.?);
                    try self.idToTypeScheme.put(let.name.lexeme, let.actualType.?);
                },
                .type => |*t| {
                    const constructorInsts = try self.allocator.alloc(
                        std.ArrayList([]*Type),
                        t.constructors.items.len,
                    );
                    errdefer self.allocator.free(constructorInsts);
                    var i: usize = 0;
                    {
                        while (i < constructorInsts.len) : (i += 1) {
                            constructorInsts[i] = .empty;
                            try self.idToInstantiations.put(
                                t.constructors.items[i].name.lexeme,
                                &constructorInsts[i],
                            );
                            try self.idToTypeScheme.put(
                                t.constructors.items[i].name.lexeme,
                                t.constructorTypeSchemes.?.items[i],
                            );
                        }
                    }
                    t.constructorInstantiations = constructorInsts;
                },
            }
        }

        // Consider Cons and Nil for lists of type Char to be used
        // And also Some and None for Ints and Float respectively
        // because of builtin functions
        var charInst = try self.allocator.alloc(*Type, 1);
        defer self.allocator.free(charInst);
        charInst[0] = self.charType;
        const consTypes = self.idToInstantiations.get("Cons").?;
        var present = false;
        for (consTypes.items) |consInst| {
            if (equalInstantiation(consInst, charInst)) {
                present = true;
            }
        }
        if (!present) {
            const copiedCharInst = try self.allocator.dupe(*Type, charInst);
            errdefer self.allocator.free(copiedCharInst);
            try consTypes.append(self.allocator, copiedCharInst);
        }
        const nilTypes = self.idToInstantiations.get("Nil").?;
        present = false;
        for (nilTypes.items) |nilInst| {
            if (equalInstantiation(nilInst, charInst)) {
                present = true;
            }
        }
        if (!present) {
            const copiedCharInst = try self.allocator.dupe(*Type, charInst);
            errdefer self.allocator.free(copiedCharInst);
            try nilTypes.append(self.allocator, copiedCharInst);
        }

        const intInst = try self.allocator.alloc(*Type, 1);
        defer self.allocator.free(intInst);
        intInst[0] = self.intType;
        const floatInst = try self.allocator.alloc(*Type, 1);
        defer self.allocator.free(floatInst);
        floatInst[0] = self.floatType;

        const someTypes = self.idToInstantiations.get("Some").?;
        var intFound = false;
        var floatFound = false;
        for (someTypes.items) |someInst| {
            if (equalInstantiation(someInst, intInst)) {
                intFound = true;
            }
            if (equalInstantiation(someInst, floatInst)) {
                floatFound = true;
            }
        }
        if (!intFound) {
            const copiedIntInst = try self.allocator.dupe(*Type, intInst);
            errdefer self.allocator.free(copiedIntInst);
            try someTypes.append(self.allocator, copiedIntInst);
        }
        if (!floatFound) {
            const copiedFloatInst = try self.allocator.dupe(*Type, floatInst);
            errdefer self.allocator.free(copiedFloatInst);
            try someTypes.append(self.allocator, copiedFloatInst);
        }

        const noneTypes = self.idToInstantiations.get("None").?;
        intFound = false;
        floatFound = false;
        for (noneTypes.items) |noneInst| {
            if (equalInstantiation(noneInst, intInst)) {
                intFound = true;
            }
            if (equalInstantiation(noneInst, floatInst)) {
                floatFound = true;
            }
        }
        if (!intFound) {
            const copiedIntInst = try self.allocator.dupe(*Type, intInst);
            errdefer self.allocator.free(copiedIntInst);
            try noneTypes.append(self.allocator, copiedIntInst);
        }
        if (!floatFound) {
            const copiedFloatInst = try self.allocator.dupe(*Type, floatInst);
            errdefer self.allocator.free(copiedFloatInst);
            try noneTypes.append(self.allocator, copiedFloatInst);
        }

        // Consider main instantiated
        const mainId = try self.allocator.create(AST);
        mainType.rc += 1;
        mainId.* = .{
            .identifier = .{
                .token = .{
                    .start = 0,
                    .end = 0,
                    .lexeme = "main",
                    .type = .Identifier,
                },
                .idType = mainType,
            },
        };
        defer mainId.deinit(self.allocator);
        try self.monomorphizeExpr(mainId);

        for (0..file.items.len) |i| {
            const statement = &file.items[file.items.len - i - 1];
            switch (statement.*) {
                .let => |*let| {
                    try self.monomorphizeDefinitions(
                        let.actualType.?,
                        let.name.lexeme,
                        let.be,
                        &let.instantiations.?,
                        &let.monomorphizations,
                    );
                },
                .type => |*t| {
                    if (t.typeArgs.items.len != 0) {
                        t.monomorphizations = .empty;
                        for (t.constructorInstantiations.?, 0..) |insts, j| {
                            const variables = t.constructorTypeSchemes.?.items[j].*.forall.typeVars.keys();
                            instLoop: for (insts.items, 0..) |inst, k| {
                                var monoConstructor = try self.monomorphizeConstructor(
                                    t.constructors.items[j],
                                    variables,
                                    inst,
                                );
                                monoConstructor.name.lexeme = try std.fmt.allocPrint(
                                    self.allocator,
                                    "_{d}{s}",
                                    .{ k, monoConstructor.name.lexeme },
                                );
                                errdefer self.allocator.free(monoConstructor.name.lexeme);
                                errdefer monoConstructor.args.deinit(self.allocator);
                                errdefer for (monoConstructor.args.items) |arg| {
                                    arg.deinit(self.allocator);
                                };
                                for (t.monomorphizations.?.items) |*monomorphization| {
                                    if (equalInstantiation(inst, monomorphization.inst)) {
                                        try monomorphization.constructors.append(
                                            self.allocator,
                                            monoConstructor,
                                        );
                                        continue :instLoop;
                                    }
                                }
                                var constructors = std.ArrayList(Statement.Constructor).empty;
                                try constructors.append(self.allocator, monoConstructor);
                                try t.monomorphizations.?.append(self.allocator, .{
                                    .inst = inst,
                                    .constructors = constructors,
                                });
                            }
                        }
                    }
                },
            }
        }

        return self.allocator.dupe(
            u8,
            mainId.identifier.token.lexeme,
        );
    }

    fn monomorphizeConstructor(
        self: *Monomorphizer,
        constructor: Statement.Constructor,
        variables: []usize,
        groundTypes: []*Type,
    ) !Statement.Constructor {
        var newArgs = try std.ArrayList(*Type).initCapacity(
            self.allocator,
            constructor.args.items.len,
        );
        for (constructor.args.items) |arg| {
            const monoArg = try self.monomorphizeType(arg, variables, groundTypes);
            errdefer monoArg.deinit(self.allocator);
            try newArgs.append(self.allocator, monoArg);
        }
        return .{ .name = constructor.name, .args = newArgs };
    }

    pub fn monomorphizeExpr(self: *Monomorphizer, expr: *AST) anyerror!void {
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
                            for (instantiations.items, 0..) |instantiation, i| {
                                if (!equalInstantiation(instantiation, types)) {
                                    continue;
                                }
                                present = true;
                                index = i;
                                break;
                            }
                            if (!present) {
                                try instantiations.append(self.allocator, types);
                            }
                            id.token.lexeme = try std.fmt.allocPrint(
                                self.allocator,
                                "_{d}{s}",
                                .{ index, id.token.lexeme },
                            );
                            if (present) {
                                self.allocator.free(types);
                            }
                        },
                        else => {},
                    }
                }
            },
            .call => |call| {
                try self.monomorphizeExpr(call.arg);
                try self.monomorphizeExpr(call.function);
            },
            .lambda => |lambda| {
                try self.monomorphizeExpr(lambda.expr);
            },
            .lambdaMult, .callMult => {},
            .let => |*let| {
                let.instantiations = .empty;
                try self.idToInstantiations.put(let.name.lexeme, &let.instantiations.?);
                try self.idToTypeScheme.put(let.name.lexeme, let.actualType.?);
                try self.monomorphizeExpr(let.in);
                _ = self.idToInstantiations.remove(let.name.lexeme);
                _ = self.idToTypeScheme.remove(let.name.lexeme);
                try self.monomorphizeDefinitions(
                    let.actualType.?,
                    let.name.lexeme,
                    let.be,
                    &let.instantiations.?,
                    &let.monomorphizations,
                );
            },
            .operator => |op| {
                try self.monomorphizeExpr(op.right);
                try self.monomorphizeExpr(op.left);
            },
            .prefixOp => |op| {
                try self.monomorphizeExpr(op.expr);
            },
            .ifExpr => |ifExpr| {
                try self.monomorphizeExpr(ifExpr.elseExpr);
                try self.monomorphizeExpr(ifExpr.thenExpr);
                try self.monomorphizeExpr(ifExpr.predicate);
            },
            .case => |case| {
                try self.monomorphizeExpr(case.value);
                for (case.bodies.items) |body| {
                    try self.monomorphizeExpr(body);
                }
            },
            .list => {},
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
            .case => |case| {
                const copiedValueType = try self.monomorphizeType(case.valueType.?, variables, groundTypes);
                errdefer copiedValueType.deinit(self.allocator);
                const copiedResultType = try self.monomorphizeType(case.resultType.?, variables, groundTypes);
                errdefer copiedResultType.deinit(self.allocator);
                const copiedValue = try self.monomorphizeAST(case.value, variables, groundTypes);
                errdefer copiedValue.deinit(self.allocator);
                var copiedPatterns = try std.ArrayList(Pattern).initCapacity(
                    self.allocator,
                    case.patterns.items.len,
                );
                errdefer copiedPatterns.deinit(self.allocator);
                errdefer for (copiedPatterns.items) |*pattern| {
                    pattern.values.deinit(self.allocator);
                    for (pattern.types.?.items) |t| {
                        t.deinit(self.allocator);
                    }
                    pattern.types.?.deinit(self.allocator);
                };
                for (case.patterns.items) |pattern| {
                    var copiedValues = try pattern.values.clone(self.allocator);
                    errdefer copiedValues.deinit(self.allocator);
                    var copiedTypes = try std.ArrayList(*Type).initCapacity(
                        self.allocator,
                        copiedValues.items.len,
                    );
                    errdefer copiedTypes.deinit(self.allocator);
                    errdefer for (copiedTypes.items) |t| {
                        t.deinit(self.allocator);
                    };
                    for (pattern.types.?.items) |t| {
                        const copiedType = try self.monomorphizeType(t, variables, groundTypes);
                        errdefer copiedType.deinit(self.allocator);
                        try copiedTypes.append(self.allocator, copiedType);
                    }
                    try copiedPatterns.append(self.allocator, .{
                        .name = pattern.name,
                        .values = copiedValues,
                        .types = copiedTypes,
                    });
                }
                var copiedBodies = try std.ArrayList(*AST).initCapacity(
                    self.allocator,
                    case.bodies.items.len,
                );
                errdefer copiedBodies.deinit(self.allocator);
                errdefer for (copiedBodies.items) |body| {
                    body.deinit(self.allocator);
                };
                for (case.bodies.items) |body| {
                    const copiedBody = try self.monomorphizeAST(
                        body,
                        variables,
                        groundTypes,
                    );
                    errdefer copiedBody.deinit(self.allocator);
                    try copiedBodies.append(self.allocator, copiedBody);
                }
                copiedAST.* = .{ .case = .{
                    .start = case.start,
                    .value = copiedValue,
                    .valueType = copiedValueType,
                    .resultType = copiedResultType,
                    .patterns = copiedPatterns,
                    .bodies = copiedBodies,
                } };
            },
            .list => {},
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
                var copiedArgs = std.ArrayList(*Type).empty;
                errdefer for (copiedArgs.items) |copied| {
                    copied.deinit(self.allocator);
                };
                errdefer copiedArgs.deinit(self.allocator);
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
                    try copiedArgs.append(self.allocator, copiedArg);
                }
                if (notCopied) {
                    for (copiedArgs.items) |arg| {
                        arg.rc -= 1;
                    }
                    copiedArgs.deinit(self.allocator);
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

    pub fn equalInstantiation(inst1: []*Type, inst2: []*Type) bool {
        for (inst1, inst2) |t1, t2| {
            if (!equalTypes(t1, t2)) {
                return false;
            }
        }
        return true;
    }

    pub fn equalTypes(left: *Type, right: *Type) bool {
        switch (right.data) {
            .typeVar => |tV1| {
                if (tV1.subst) |subst| {
                    return equalTypes(left, subst);
                }
            },
            else => {},
        }
        switch (left.data) {
            .typeVar => |tV1| {
                if (tV1.subst) |subst| {
                    return equalTypes(subst, right);
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
                        return equalTypes(left, number.variable);
                    },
                    else => {
                        return false;
                    },
                }
            },
            .function => |function1| {
                switch (right.data) {
                    .function => |function2| {
                        return equalTypes(function1.from, function2.from) and equalTypes(function1.to, function2.to);
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
                            if (!equalTypes(type1, type2)) {
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
                        return equalTypes(number1.variable, number2.variable);
                    },
                    else => {
                        return false;
                    },
                }
            },
        }
    }
};
