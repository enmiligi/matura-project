const std = @import("std");
const AST = @import("./ast.zig").AST;

const PrimitiveType = enum { Int, Float };

pub const TypeVar = usize;

pub const Type = union(enum) {
    typeVar: TypeVar,
    primitive: PrimitiveType,
    function: struct {
        from: *Type,
        to: *Type,
    },
};

pub const TypeScheme = union(enum) {
    type: Type,
    forall: struct {
        typeVar: TypeVar,
        scheme: *TypeScheme,
    },
};

const Substitution = std.AutoHashMap(TypeVar, *Type);

const TypeEnv = std.StringHashMap(*Type);

pub const InferenceError = error{
    CouldNotUnify,
    InfiniteType,
    UnknownIdentifier,
};

pub const AlgorithmJ = struct {
    substE: Substitution,
    currentTypeVar: TypeVar = 0,
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) AlgorithmJ {
        return .{
            .substE = Substitution.init(allocator),
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *AlgorithmJ) void {
        self.substE.deinit();
    }

    pub fn getType(self: *AlgorithmJ, expr: *AST) !*Type {
        var typeEnv = TypeEnv.init(self.allocator);
        defer typeEnv.deinit();
        return self.substitute(try self.run(&typeEnv, expr));
    }

    fn newVar(self: *AlgorithmJ) TypeVar {
        self.currentTypeVar += 1;
        return self.currentTypeVar;
    }

    fn substituteTypeVar(typeA: *Type, typeVarA: TypeVar, typeB: *Type) *Type {
        switch (typeA.*) {
            .typeVar => |typeVar| {
                if (typeVar == typeVarA) {
                    return typeB;
                } else {
                    return typeA;
                }
            },
            .primitive => |_| {
                return typeA;
            },
            .function => |*function| {
                function.from = substituteTypeVar(function.from, typeVarA, typeB);
                function.to = substituteTypeVar(function.to, typeVarA, typeB);
                return typeA;
            },
        }
    }

    fn substitute(self: *AlgorithmJ, typeA: *Type) *Type {
        switch (typeA.*) {
            .typeVar => |typeVarA| {
                return self.substE.get(typeVarA) orelse typeA;
            },
            .function => |*functionA| {
                functionA.from = self.substitute(functionA.from);
                functionA.to = self.substitute(functionA.to);
                return typeA;
            },
            else => {
                return typeA;
            },
        }
    }

    fn contains(typeVarA: TypeVar, typeB: *Type) bool {
        switch (typeB.*) {
            .typeVar => |typeVarB| {
                return typeVarA == typeVarB;
            },
            .function => |functionB| {
                return contains(typeVarA, functionB.from) or contains(typeVarA, functionB.to);
            },
            .primitive => |_| {
                return false;
            },
        }
    }

    fn addSubstitution(self: *AlgorithmJ, typeVarA: TypeVar, typeB: *Type) !void {
        switch (typeB.*) {
            .typeVar => |typeVarB| {
                if (typeVarA != typeVarB) {
                    try self.substE.put(typeVarA, typeB);
                } else {
                    return;
                }
            },
            .primitive => |_| {
                try self.substE.put(typeVarA, typeB);
            },
            .function => |_| {
                if (contains(typeVarA, typeB)) {
                    return error.InfiniteType;
                } else {
                    try self.substE.put(typeVarA, typeB);
                }
            },
        }
        var iterator = self.substE.iterator();
        while (iterator.next()) |entry| {
            const substituted = substituteTypeVar(entry.value_ptr.*, typeVarA, typeB);
            entry.value_ptr.* = substituted;
        }
    }

    fn unify(self: *AlgorithmJ, typeA: *Type, typeB: *Type) !void {
        switch (typeA.*) {
            .typeVar => |typeVarA| {
                if (self.substE.get(typeVarA)) |substitutedA| {
                    try self.unify(substitutedA, typeB);
                } else {
                    try self.addSubstitution(typeVarA, typeB);
                }
            },
            .primitive => |primitiveA| {
                switch (typeB.*) {
                    .typeVar => |typeVarB| {
                        if (self.substE.get(typeVarB)) |substitutedB| {
                            try self.unify(typeA, substitutedB);
                        } else {
                            try self.addSubstitution(typeVarB, typeA);
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
                switch (typeB.*) {
                    .primitive => |_| {
                        return error.CouldNotUnify;
                    },
                    .function => |functionB| {
                        try self.unify(functionA.from, functionB.from);
                        try self.unify(functionA.to, functionB.to);
                    },
                    .typeVar => |typeVarB| {
                        if (self.substE.get(typeVarB)) |substitutedB| {
                            try self.unify(typeA, substitutedB);
                        } else {
                            try self.addSubstitution(typeVarB, typeA);
                        }
                    },
                }
            },
        }
    }

    fn run(self: *AlgorithmJ, typeEnv: *TypeEnv, ast: *AST) !*Type {
        const t = try self.allocator.create(Type);
        switch (ast.*) {
            .identifier => |id| {
                defer self.allocator.destroy(t);
                return typeEnv.get(id.token.lexeme) orelse error.UnknownIdentifier;
            },
            .intConstant => |_| {
                t.* = .{ .primitive = .Int };
            },
            .floatConstant => |_| {
                t.* = .{ .primitive = .Float };
            },
            .call => |call| {
                const t1 = try self.run(typeEnv, call.function);
                const t2 = try self.run(typeEnv, call.arg);
                t.* = .{ .typeVar = self.newVar() };
                const fType = try self.allocator.create(Type);
                errdefer self.allocator.destroy(fType);
                fType.* = .{ .function = .{
                    .from = t2,
                    .to = t,
                } };
                try self.unify(t1, fType);
            },
            .lambda => |lambda| {
                const newTypeVar = try self.allocator.create(Type);
                newTypeVar.* = .{ .typeVar = self.newVar() };
                errdefer self.allocator.destroy(newTypeVar);
                const previous = typeEnv.get(lambda.argname.lexeme);
                try typeEnv.put(lambda.argname.lexeme, newTypeVar);
                const returnType = try self.run(typeEnv, lambda.expr);
                if (previous) |previousType| {
                    try typeEnv.put(lambda.argname.lexeme, previousType);
                } else {
                    _ = typeEnv.remove(lambda.argname.lexeme);
                }
                t.* = .{ .function = .{
                    .from = newTypeVar,
                    .to = returnType,
                } };
            },
            .let => |let| {
                defer self.allocator.destroy(t);
                const typeOfVar = try self.run(typeEnv, let.be);
                const previous = typeEnv.get(let.name.lexeme);
                try typeEnv.put(let.name.lexeme, typeOfVar);
                const typeOfExpr = self.run(typeEnv, let.in);
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
