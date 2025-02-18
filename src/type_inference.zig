const std = @import("std");
const AST = @import("./ast.zig").AST;

const PrimitiveType = enum { Int, Float };

pub const TypeVar = struct {
    n: usize,
    subst: ?*Type,
};

pub const Type = struct {
    data: union(enum) {
        typeVar: TypeVar,
        primitive: PrimitiveType,
        function: struct {
            from: *Type,
            to: *Type,
        },
    },
    rc: usize,

    pub fn init(allocator: std.mem.Allocator) !*Type {
        const t = try allocator.create(Type);
        t.*.rc = 1;
        return t;
    }

    pub fn deinit(self: *Type, allocator: std.mem.Allocator) void {
        self.rc -= 1;
        if (self.rc == 0) {
            switch (self.data) {
                .typeVar => |typeVar| {
                    if (typeVar.subst) |subst| {
                        subst.deinit(allocator);
                    }
                },
                .primitive => {},
                .function => |function| {
                    function.from.deinit(allocator);
                    function.to.deinit(allocator);
                },
            }
            allocator.destroy(self);
        }
    }
};

pub fn debugPrintType(t: *Type) void {
    switch (t.data) {
        .typeVar => |typeVar| {
            if (typeVar.subst) |substitution| {
                debugPrintType(substitution);
            } else {
                std.debug.print("TypeVar({d}, rc: {d})", .{ typeVar.n, t.rc });
            }
        },
        .primitive => |primitive| {
            switch (primitive) {
                .Int => {
                    std.debug.print("Int(rc: {d})", .{t.rc});
                },
                .Float => {
                    std.debug.print("Float(rc: {d})", .{t.rc});
                },
            }
        },
        .function => |function| {
            std.debug.print("(", .{});
            debugPrintType(function.from);
            std.debug.print(" -> ", .{});
            debugPrintType(function.to);
            std.debug.print(", rc: {d})", .{t.rc});
        },
    }
}

const TypeVarSet = std.AutoHashMap(usize, void);

pub const Forall = struct {
    typeVars: TypeVarSet,
    type: *Type,
};

pub const TypeScheme = union(enum) {
    type: *Type,
    forall: Forall,
};

pub fn deinitScheme(scheme: *TypeScheme, allocator: std.mem.Allocator) void {
    switch (scheme.*) {
        .type => |t| {
            t.deinit(allocator);
        },
        .forall => |*forall| {
            forall.typeVars.deinit();
            forall.type.deinit(allocator);
        },
    }
    allocator.destroy(scheme);
}

const TypeEnv = std.StringHashMap(*TypeScheme);

fn deinitTypeEnv(env: *TypeEnv, allocator: std.mem.Allocator) void {
    var iterator = env.iterator();
    while (iterator.next()) |entry| {
        deinitScheme(entry.value_ptr.*, allocator);
    }
    env.deinit();
}

pub const InferenceError = error{
    CouldNotUnify,
    InfiniteType,
    UnknownIdentifier,
};

pub const AlgorithmJ = struct {
    currentTypeVar: usize = 0,
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) AlgorithmJ {
        return .{
            .allocator = allocator,
        };
    }

    pub fn getType(self: *AlgorithmJ, expr: *AST) !*Type {
        var typeEnv = TypeEnv.init(self.allocator);
        defer deinitTypeEnv(&typeEnv, self.allocator);
        return self.run(&typeEnv, expr);
    }

    fn newVar(self: *AlgorithmJ) TypeVar {
        self.currentTypeVar += 1;
        return .{
            .n = self.currentTypeVar,
            .subst = null,
        };
    }

    fn newVarT(self: *AlgorithmJ) Type {
        return .{ .data = .{ .typeVar = self.newVar() }, .rc = 1 };
    }

    fn contains(typeVarA: *TypeVar, typeB: *Type) bool {
        switch (typeB.data) {
            .typeVar => |typeVarB| {
                if (typeVarB.subst) |substB| {
                    return contains(typeVarA, substB);
                } else {
                    return typeVarA.*.n == typeVarB.n;
                }
            },
            .function => |functionB| {
                return contains(typeVarA, functionB.from) or contains(typeVarA, functionB.to);
            },
            .primitive => |_| {
                return false;
            },
        }
    }

    fn addSubstitution(typeVarA: *TypeVar, typeB: *Type) !void {
        if (contains(typeVarA, typeB)) {
            return error.InfiniteType;
        }
        typeB.rc += 1;
        typeVarA.subst = typeB;
    }

    fn unify(self: *AlgorithmJ, typeA: *Type, typeB: *Type) !void {
        switch (typeA.data) {
            .typeVar => |*typeVarA| {
                if (typeVarA.subst) |substitutedA| {
                    try self.unify(substitutedA, typeB);
                } else {
                    switch (typeB.data) {
                        .typeVar => |_| {
                            if (typeA == typeB) {
                                return;
                            }
                        },
                        else => {},
                    }
                    try addSubstitution(typeVarA, typeB);
                }
            },
            .primitive => |primitiveA| {
                switch (typeB.data) {
                    .typeVar => |*typeVarB| {
                        if (typeVarB.subst) |substitutedB| {
                            try self.unify(typeA, substitutedB);
                        } else {
                            try addSubstitution(typeVarB, typeA);
                        }
                    },
                    .primitive => |primitiveB| {
                        if (primitiveA != primitiveB) {
                            return error.CouldNotUnify;
                        }
                    },
                    .function => |_| {
                        return error.CouldNotUnify;
                    },
                }
            },
            .function => |functionA| {
                switch (typeB.data) {
                    .primitive => |_| {
                        return error.CouldNotUnify;
                    },
                    .function => |functionB| {
                        try self.unify(functionA.from, functionB.from);
                        try self.unify(functionA.to, functionB.to);
                    },
                    .typeVar => |*typeVarB| {
                        if (typeVarB.subst) |substitutedB| {
                            try self.unify(typeA, substitutedB);
                        } else {
                            try addSubstitution(typeVarB, typeA);
                        }
                    },
                }
            },
        }
    }

    fn copyAndReplace(self: *AlgorithmJ, inputType: *Type, replacementMap: *const std.AutoHashMap(usize, *Type)) !*Type {
        switch (inputType.data) {
            .typeVar => |typeVar| {
                if (typeVar.subst) |subst| {
                    return self.copyAndReplace(subst, replacementMap);
                } else {
                    if (replacementMap.get(typeVar.n)) |newTypeVar| {
                        newTypeVar.rc += 1;
                        return newTypeVar;
                    } else {
                        inputType.rc += 1;
                        return inputType;
                    }
                }
            },
            .primitive => |primitive| {
                const t = try Type.init(self.allocator);
                t.data = .{ .primitive = primitive };
                return t;
            },
            .function => |function| {
                const t = try Type.init(self.allocator);
                errdefer self.allocator.destroy(t);
                const fromCopied = try self.copyAndReplace(function.from, replacementMap);
                errdefer fromCopied.deinit(self.allocator);
                t.data = .{ .function = .{
                    .from = fromCopied,
                    .to = try self.copyAndReplace(function.to, replacementMap),
                } };
                return t;
            },
        }
    }

    fn instantiate(self: *AlgorithmJ, forall: *Forall) !*Type {
        var instantiatedVars = std.AutoHashMap(usize, *Type).init(self.allocator);
        defer instantiatedVars.deinit();

        var iterator = forall.typeVars.iterator();
        {
            errdefer {
                var instIterator = instantiatedVars.iterator();
                while (instIterator.next()) |entry| {
                    self.allocator.destroy(entry.value_ptr.*);
                }
            }
            while (iterator.next()) |entry| {
                const t = try self.allocator.create(Type);
                errdefer self.allocator.destroy(t);
                t.* = self.newVarT();
                try instantiatedVars.put(entry.key_ptr.*, t);
            }
        }
        defer {
            var instIterator = instantiatedVars.iterator();
            while (instIterator.next()) |entry| {
                entry.value_ptr.*.deinit(self.allocator);
            }
            instantiatedVars.deinit();
        }
        return try self.copyAndReplace(forall.type, &instantiatedVars);
    }

    fn run(self: *AlgorithmJ, typeEnv: *TypeEnv, ast: *AST) !*Type {
        const t = try Type.init(self.allocator);
        switch (ast.*) {
            .identifier => |id| {
                self.allocator.destroy(t);
                if (typeEnv.get(id.token.lexeme)) |typeOfIdScheme| {
                    switch (typeOfIdScheme.*) {
                        .type => |typeOfId| {
                            typeOfId.rc += 1;
                            return typeOfId;
                        },
                        .forall => |*forall| {
                            return self.instantiate(forall);
                        },
                    }
                } else {
                    return error.UnknownIdentifier;
                }
            },
            .intConstant => |_| {
                t.data = .{ .primitive = .Int };
            },
            .floatConstant => |_| {
                t.data = .{ .primitive = .Float };
            },
            .call => |call| {
                t.* = self.newVarT();
                errdefer t.deinit(self.allocator);
                const t1 = try self.run(typeEnv, call.function);
                defer t1.deinit(self.allocator);
                const t2 = try self.run(typeEnv, call.arg);
                defer t2.deinit(self.allocator);
                const fType = try Type.init(self.allocator);
                defer self.allocator.destroy(fType);
                fType.data = .{ .function = .{
                    .from = t2,
                    .to = t,
                } };
                try self.unify(t1, fType);
            },
            .lambda => |lambda| {
                errdefer self.allocator.destroy(t);
                const newTypeVar = try Type.init(self.allocator);
                newTypeVar.* = self.newVarT();
                var typeScheme: *TypeScheme = undefined;
                {
                    errdefer newTypeVar.deinit(self.allocator);
                    typeScheme = try self.allocator.create(TypeScheme);
                }
                typeScheme.* = .{ .type = newTypeVar };
                const previous = typeEnv.get(lambda.argname.lexeme);
                if (previous) |pT| {
                    errdefer deinitScheme(pT, self.allocator);
                }
                {
                    errdefer self.allocator.destroy(typeScheme);
                    errdefer newTypeVar.deinit(self.allocator);
                    try typeEnv.put(lambda.argname.lexeme, typeScheme);
                }
                const returnType = try self.run(typeEnv, lambda.expr);
                errdefer returnType.deinit(self.allocator);
                if (previous) |previousType| {
                    try typeEnv.put(lambda.argname.lexeme, previousType);
                } else {
                    _ = typeEnv.remove(lambda.argname.lexeme);
                }
                self.allocator.destroy(typeScheme);
                t.data = .{ .function = .{
                    .from = newTypeVar,
                    .to = returnType,
                } };
            },
            .let => |let| {
                self.allocator.destroy(t);
                const typeOfVar = try self.run(typeEnv, let.be);
                const previous = typeEnv.get(let.name.lexeme);
                var typeScheme: *TypeScheme = undefined;
                {
                    errdefer typeOfVar.deinit(self.allocator);
                    typeScheme = try self.allocator.create(TypeScheme);
                    typeScheme.* = .{ .type = typeOfVar };
                    errdefer self.allocator.destroy(typeScheme);
                    try typeEnv.put(let.name.lexeme, typeScheme);
                }
                if (previous) |pT| {
                    errdefer pT.deinit(self.allocator);
                }
                const typeOfExpr = try self.run(typeEnv, let.in);
                errdefer typeOfExpr.deinit(self.allocator);
                if (previous) |previousType| {
                    try typeEnv.put(let.name.lexeme, previousType);
                } else {
                    _ = typeEnv.remove(let.name.lexeme);
                }
                deinitScheme(typeScheme, self.allocator);
                return typeOfExpr;
            },
        }
        return t;
    }
};
