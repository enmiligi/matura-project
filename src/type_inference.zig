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

pub const TypeScheme = union(enum) {
    type: Type,
    forall: struct {
        typeVar: TypeVar,
        scheme: *TypeScheme,
    },
};

const TypeEnv = std.StringHashMap(*Type);

fn deinitTypeEnv(env: *TypeEnv, allocator: std.mem.Allocator) void {
    var iterator = env.iterator();
    while (iterator.next()) |entry| {
        entry.value_ptr.*.deinit(allocator);
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

    fn run(self: *AlgorithmJ, typeEnv: *TypeEnv, ast: *AST) !*Type {
        const t = try Type.init(self.allocator);
        switch (ast.*) {
            .identifier => |id| {
                self.allocator.destroy(t);
                if (typeEnv.get(id.token.lexeme)) |typeOfId| {
                    typeOfId.rc += 1;
                    return typeOfId;
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
                errdefer newTypeVar.deinit(self.allocator);
                const previous = typeEnv.get(lambda.argname.lexeme);
                try typeEnv.put(lambda.argname.lexeme, newTypeVar);
                const returnType = try self.run(typeEnv, lambda.expr);
                errdefer returnType.deinit(self.allocator);
                if (previous) |previousType| {
                    try typeEnv.put(lambda.argname.lexeme, previousType);
                } else {
                    _ = typeEnv.remove(lambda.argname.lexeme);
                }
                t.data = .{ .function = .{
                    .from = newTypeVar,
                    .to = returnType,
                } };
            },
            .let => |let| {
                self.allocator.destroy(t);
                const typeOfVar = try self.run(typeEnv, let.be);
                const previous = typeEnv.get(let.name.lexeme);
                {
                    errdefer typeOfVar.deinit(self.allocator);
                    try typeEnv.put(let.name.lexeme, typeOfVar);
                }
                if (previous) |pT| {
                    errdefer pT.deinit(self.allocator);
                }
                const typeOfExpr = try self.run(typeEnv, let.in);
                defer typeOfVar.deinit(self.allocator);
                errdefer typeOfExpr.deinit(self.allocator);
                if (previous) |previousType| {
                    try typeEnv.put(let.name.lexeme, previousType);
                } else {
                    _ = typeEnv.remove(let.name.lexeme);
                }
                return typeOfExpr;
            },
        }
        return t;
    }
};
