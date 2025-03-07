const std = @import("std");
const AST = @import("./ast.zig").AST;

const PrimitiveType = enum { Int, Float };

// TypeVars are identified by their number
// The depth assigned is used to efficiently check if you can generalise
// Substitutions are stored in the TypeVar instead of a global Substitution map.
pub const TypeVar = struct {
    n: usize,
    depth: usize,
    subst: ?*Type,
};

// This is the type used for non-generalised/instatiated types
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

pub fn printType(t: *Type, writer: std.io.AnyWriter) !void {
    switch (t.data) {
        .typeVar => |typeVar| {
            if (typeVar.subst) |substitution| {
                try printType(substitution, writer);
            } else {
                try writer.print("TypeVar({d}, rc: {d})", .{ typeVar.n, t.rc });
            }
        },
        .primitive => |primitive| {
            switch (primitive) {
                .Int => {
                    try writer.print("Int(rc: {d})", .{t.rc});
                },
                .Float => {
                    try writer.print("Float(rc: {d})", .{t.rc});
                },
            }
        },
        .function => |function| {
            try writer.print("(", .{});
            try printType(function.from, writer);
            try writer.print(" -> ", .{});
            try printType(function.to, writer);
            try writer.print(", rc: {d})", .{t.rc});
        },
    }
}

const TypeVarSet = std.AutoHashMap(usize, void);

// The type for generalised types
pub const Forall = struct {
    typeVars: TypeVarSet,
    type: *Type,
};

// The type of optionally generalised types
pub const TypeScheme = union(enum) {
    type: *Type,
    forall: Forall,
};

// Deinitialize TypeScheme
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

// Deinitialize stored types in typeEnv
fn deinitTypeEnv(env: *TypeEnv, allocator: std.mem.Allocator) void {
    var iterator = env.iterator();
    while (iterator.next()) |entry| {
        deinitScheme(entry.value_ptr.*, allocator);
        env.removeByPtr(entry.key_ptr);
    }
}

pub const InferenceError = error{
    CouldNotUnify,
    InfiniteType,
    UnknownIdentifier,
};

// The struct containing all global data used for Algorithm J
// except the substitutions
pub const AlgorithmJ = struct {
    currentTypeVar: usize = 0,
    allocator: std.mem.Allocator,
    depth: usize = 0,

    pub fn init(allocator: std.mem.Allocator) AlgorithmJ {
        return .{
            .allocator = allocator,
        };
    }

    pub fn getType(self: *AlgorithmJ, expr: *AST) !*Type {
        var typeEnv = TypeEnv.init(self.allocator);
        defer typeEnv.deinit();
        return self.run(&typeEnv, expr);
    }

    fn newVar(self: *AlgorithmJ) TypeVar {
        self.currentTypeVar += 1;
        return .{
            .n = self.currentTypeVar,
            .depth = self.depth,
            .subst = null,
        };
    }

    fn newVarT(self: *AlgorithmJ) Type {
        return .{ .data = .{ .typeVar = self.newVar() }, .rc = 1 };
    }

    // This function is used for checking if there is an infinite type
    // and also adjust the depth since typeVarA will be substituted with
    // something containing typeB if not to not have two traversals
    fn containsAndShiftDepth(typeVarA: *TypeVar, typeB: *Type) bool {
        switch (typeB.data) {
            .typeVar => |*typeVarB| {
                if (typeVarB.subst) |substB| {
                    return containsAndShiftDepth(typeVarA, substB);
                } else {
                    if (typeVarA.depth < typeVarB.depth) {
                        typeVarB.depth = typeVarA.depth;
                    }
                    return typeVarA.*.n == typeVarB.n;
                }
            },
            .function => |functionB| {
                return containsAndShiftDepth(typeVarA, functionB.from) or
                    containsAndShiftDepth(typeVarA, functionB.to);
            },
            .primitive => |_| {
                return false;
            },
        }
    }

    fn addSubstitution(typeVarA: *TypeVar, typeB: *Type) !void {
        if (containsAndShiftDepth(typeVarA, typeB)) {
            return error.InfiniteType;
        }
        typeB.rc += 1;
        typeVarA.subst = typeB;
    }

    // Unify typeA and typeB creating substitutions as needed
    fn unify(self: *AlgorithmJ, typeA: *Type, typeB: *Type) !void {
        switch (typeA.data) {
            .typeVar => |*typeVarA| {
                // If A is a typeVar, unify its substitution if present
                // else add a Substitution unless 'a' should be replaced with 'a'
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

    // Recursively copy the Type while replacing type variables
    fn copyAndReplace(self: *AlgorithmJ, inputType: *Type, replacementMap: *const std.AutoHashMap(usize, *Type)) !*Type {
        switch (inputType.data) {
            .typeVar => |typeVar| {
                if (typeVar.subst) |subst| {
                    const t = try Type.init(self.allocator);
                    errdefer self.allocator.destroy(t);
                    t.data = .{ .typeVar = .{
                        .n = typeVar.n,
                        .depth = typeVar.depth,
                        .subst = try self.copyAndReplace(subst, replacementMap),
                    } };
                    return t;
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

    // Create a replacement map and apply it
    fn instantiate(self: *AlgorithmJ, forall: *Forall) !*Type {
        var instantiatedVars = std.AutoHashMap(usize, *Type).init(self.allocator);

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

    // Find all Free Vars contained in a Type
    fn findFreeVars(self: *AlgorithmJ, t: *Type, minDepth: usize, currentFreeVars: *TypeVarSet) !void {
        switch (t.data) {
            .typeVar => |typevar| {
                // If the type variable has too low depth,
                // it comes from a lambda binding outside
                // meaning it can not be generalised
                if (typevar.depth < minDepth) {
                    return;
                }
                if (typevar.subst) |substitution| {
                    try self.findFreeVars(substitution, minDepth, currentFreeVars);
                } else {
                    try currentFreeVars.put(typevar.n, undefined);
                }
            },
            .function => |function| {
                try self.findFreeVars(function.from, minDepth, currentFreeVars);
                try self.findFreeVars(function.to, minDepth, currentFreeVars);
            },
            .primitive => {},
        }
    }

    // Find all free vars
    fn generalise(self: *AlgorithmJ, t: *Type, minDepth: usize) !*TypeScheme {
        var freeVars = TypeVarSet.init(self.allocator);
        try self.findFreeVars(t, minDepth, &freeVars);
        errdefer freeVars.deinit();
        const typeScheme = try self.allocator.create(TypeScheme);
        typeScheme.* = .{ .forall = .{
            .typeVars = freeVars,
            .type = t,
        } };
        return typeScheme;
    }

    // The main algorithm as described in the paper
    fn run(self: *AlgorithmJ, typeEnv: *TypeEnv, ast: *AST) !*Type {
        errdefer deinitTypeEnv(typeEnv, self.allocator);
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
                self.depth += 1;
                const typeOfVarTypeVar = try Type.init(self.allocator);
                typeOfVarTypeVar.* = self.newVarT();
                const typeOfVarScheme = self.allocator.create(TypeScheme) catch |err| {
                    typeOfVarTypeVar.deinit(self.allocator);
                    return err;
                };
                typeOfVarScheme.* = .{ .type = typeOfVarTypeVar };
                const previous = typeEnv.get(let.name.lexeme);
                errdefer if (previous) |pT| {
                    deinitScheme(pT, self.allocator);
                };
                {
                    errdefer deinitScheme(typeOfVarScheme, self.allocator);
                    try typeEnv.put(let.name.lexeme, typeOfVarScheme);
                }
                const typeOfVar = try self.run(typeEnv, let.be);
                {
                    errdefer typeOfVar.deinit(self.allocator);
                    errdefer deinitScheme(typeOfVarScheme, self.allocator);
                    try self.unify(typeOfVarTypeVar, typeOfVar);
                }
                self.depth -= 1;
                const generalised = self.generalise(typeOfVar, self.depth + 1) catch |err| {
                    typeOfVar.deinit(self.allocator);
                    return err;
                };
                {
                    errdefer typeOfVar.deinit(self.allocator);
                    deinitScheme(typeOfVarScheme, self.allocator);
                    try typeEnv.put(let.name.lexeme, generalised);
                }
                const typeOfExpr = try self.run(typeEnv, let.in);
                errdefer typeOfExpr.deinit(self.allocator);
                if (previous) |previousType| {
                    try typeEnv.put(let.name.lexeme, previousType);
                } else {
                    _ = typeEnv.remove(let.name.lexeme);
                }
                deinitScheme(generalised, self.allocator);
                return typeOfExpr;
            },
        }
        return t;
    }
};
