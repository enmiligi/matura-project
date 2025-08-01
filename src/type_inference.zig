const std = @import("std");
const AST = @import("./ast.zig").AST;
const TypeAnnotation = @import("./ast.zig").TypeAnnotation;
const Statement = @import("./ast.zig").Statement;
const Errors = @import("./errors.zig").Errors;
const token = @import("token.zig");
const computeBoundaries = @import("./utils.zig").computeBoundaries;

const PrimitiveType = enum { Int, Float, Bool, Char };

pub const Number = struct {
    variable: *Type,
};

// TypeVars are identified by their number
// The depth assigned is used to efficiently check if you can generalise
// Substitutions are stored in the TypeVar instead of a global Substitution map.
pub const TypeVar = struct {
    n: usize,
    depth: usize,
    subst: ?*Type,
    declaration: bool = false,
};

// This is the type used for non-generalised/instatiated types
pub const Type = struct {
    data: union(enum) {
        typeVar: TypeVar,
        primitive: PrimitiveType,
        composite: struct {
            name: []const u8,
            args: std.ArrayList(*Type),
        },
        function: struct {
            from: *Type,
            to: *Type,
        },
        number: Number,
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
                .number => |num| {
                    num.variable.deinit(allocator);
                },
                .composite => |comp| {
                    for (comp.args.items) |arg| {
                        arg.deinit(allocator);
                    }
                    comp.args.deinit();
                },
            }
            allocator.destroy(self);
        }
    }
};

pub const Constraint = struct {
    typeVar: usize,
    name: []const u8,
};

// Gather all constraints and type variables contained in type
// in order to use them for printing
fn collectConstraintsAndTypeVars(
    t: *Type,
    constraints: *std.AutoArrayHashMap(*Type, Constraint),
    typeVarMap: *std.AutoHashMap(usize, usize),
    currentTypeVar: *usize,
    allocator: std.mem.Allocator,
) !void {
    switch (t.data) {
        .typeVar => |typeVar| {
            if (typeVar.subst) |substitution| {
                try collectConstraintsAndTypeVars(substitution, constraints, typeVarMap, currentTypeVar, allocator);
            } else {
                var varNum: usize = undefined;
                if (typeVarMap.get(typeVar.n)) |num| {
                    varNum = num;
                } else {
                    varNum = currentTypeVar.*;
                    try typeVarMap.put(typeVar.n, varNum);
                    currentTypeVar.* += 1;
                }
            }
        },
        .function => |function| {
            try collectConstraintsAndTypeVars(function.from, constraints, typeVarMap, currentTypeVar, allocator);
            try collectConstraintsAndTypeVars(function.to, constraints, typeVarMap, currentTypeVar, allocator);
        },
        .number => |*num| {
            if (num.variable.data.typeVar.subst) |subst| {
                switch (subst.data) {
                    .typeVar => {
                        subst.rc += 1;
                        num.variable.deinit(allocator);
                        num.variable = subst;
                        try collectConstraintsAndTypeVars(t, constraints, typeVarMap, currentTypeVar, allocator);
                    },
                    .number => {
                        try collectConstraintsAndTypeVars(subst, constraints, typeVarMap, currentTypeVar, allocator);
                    },
                    else => {
                        return;
                    },
                }
            } else {
                try collectConstraintsAndTypeVars(num.variable, constraints, typeVarMap, currentTypeVar, allocator);
                if (!constraints.contains(num.variable)) {
                    const varNum = typeVarMap.get(num.variable.data.typeVar.n).?;
                    try constraints.put(num.variable, .{ .name = "Number", .typeVar = varNum });
                }
            }
        },
        .composite => |composite| {
            for (composite.args.items) |arg| {
                try collectConstraintsAndTypeVars(arg, constraints, typeVarMap, currentTypeVar, allocator);
            }
        },
        else => {
            return;
        },
    }
}

// Print all the constraints that a type has
pub fn printConstraints(
    t: *Type,
    writer: std.io.AnyWriter,
    currentTypeVar: *usize,
    typeVarMap: *std.AutoHashMap(usize, usize),
    allocator: std.mem.Allocator,
) !void {
    var constraintsMap: std.AutoArrayHashMap(*Type, Constraint) = .init(allocator);
    defer constraintsMap.deinit();
    try collectConstraintsAndTypeVars(t, &constraintsMap, typeVarMap, currentTypeVar, allocator);
    var i: usize = 0;
    const constraints = constraintsMap.values();
    while (i < constraints.len) : (i += 1) {
        try writer.print("{s} ", .{constraints[i].name});
        if (constraints[i].typeVar < 26) {
            const byteVarNum: u8 = @intCast(constraints[i].typeVar);
            try writer.print("{c}", .{'a' + byteVarNum});
        } else {
            try writer.print("var_{d}", .{constraints[i].typeVar - 26});
        }
        if (i != constraints.len - 1) {
            try writer.print(", ", .{});
        }
    }
    if (constraints.len != 0) {
        try writer.print(" => ", .{});
    }
}

// Print the type with Number constraints left out
pub fn printTypeWithoutConstraints(
    t: *Type,
    writer: std.io.AnyWriter,
    currentTypeVar: *usize,
    typeVarMap: *std.AutoHashMap(usize, usize),
    topLevel: bool,
    allocator: std.mem.Allocator,
) !void {
    switch (t.data) {
        .typeVar => |typeVar| {
            if (typeVar.subst) |substitution| {
                try printTypeWithoutConstraints(substitution, writer, currentTypeVar, typeVarMap, topLevel, allocator);
            } else {
                const varNum: usize = typeVarMap.get(typeVar.n).?;
                if (varNum < 26) {
                    const byteVarNum: u8 = @intCast(varNum);
                    try writer.print("{c}", .{'a' + byteVarNum});
                } else {
                    try writer.print("var_{d}", .{varNum - 26});
                }
            }
        },
        .primitive => |primitive| {
            switch (primitive) {
                .Int => {
                    try writer.print("Int", .{});
                },
                .Float => {
                    try writer.print("Float", .{});
                },
                .Bool => {
                    try writer.print("Bool", .{});
                },
                .Char => {
                    try writer.print("Char", .{});
                },
            }
        },
        .composite => |comp| {
            try writer.print("{s}", .{comp.name});
            for (comp.args.items) |arg| {
                try writer.print(" ", .{});
                try printTypeWithoutConstraints(
                    arg,
                    writer,
                    currentTypeVar,
                    typeVarMap,
                    false,
                    allocator,
                );
            }
        },
        .function => |function| {
            if (!topLevel) try writer.print("(", .{});
            try printTypeWithoutConstraints(function.from, writer, currentTypeVar, typeVarMap, false, allocator);
            try writer.print(" -> ", .{});
            try printTypeWithoutConstraints(function.to, writer, currentTypeVar, typeVarMap, true, allocator);
            if (!topLevel) try writer.print(")", .{});
        },
        .number => |*num| {
            try printTypeWithoutConstraints(num.variable, writer, currentTypeVar, typeVarMap, true, allocator);
        },
    }
}

// Print the whole type
pub fn printType(
    t: *Type,
    writer: std.io.AnyWriter,
    currentTypeVar: *usize,
    typeVarMap: *std.AutoHashMap(usize, usize),
    topLevel: bool,
    allocator: std.mem.Allocator,
) !void {
    try printConstraints(t, writer, currentTypeVar, typeVarMap, allocator);
    try printTypeWithoutConstraints(t, writer, currentTypeVar, typeVarMap, topLevel, allocator);
}

const TypeVarSet = std.AutoArrayHashMap(usize, void);

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
    }
}

pub const InferenceError = error{
    CouldNotUnify,
    InfiniteType,
    UnknownIdentifier,
    TooGeneral,
    NonExhaustiveMatch,
};

// The struct containing all global data used for Algorithm J
// except the substitutions
pub const AlgorithmJ = struct {
    currentTypeVar: usize = 0,
    allocator: std.mem.Allocator,
    depth: usize = 0,
    errors: *Errors,

    globalTypes: TypeEnv,
    composite: std.StringHashMap(struct {
        numVars: usize,
        constructors: ?std.StringArrayHashMap(bool),
        compositeType: ?*Type,
    }),
    constructorToType: std.StringHashMap([]const u8),

    pub fn init(allocator: std.mem.Allocator, errs: *Errors) AlgorithmJ {
        return .{
            .allocator = allocator,
            .errors = errs,
            .globalTypes = .init(allocator),
            .composite = .init(allocator),
            .constructorToType = .init(allocator),
        };
    }

    pub fn deinit(self: *AlgorithmJ) void {
        deinitTypeEnv(&self.globalTypes, self.allocator);
        self.globalTypes.deinit();
        var compIter = self.composite.valueIterator();
        while (compIter.next()) |compType| {
            if (compType.constructors) |*constructors| {
                constructors.deinit();
            }
            if (compType.compositeType) |comp| {
                comp.deinit(self.allocator);
            }
        }
        self.composite.deinit();
        self.constructorToType.deinit();
    }

    // Create a new type variable with current depth
    // and number
    fn newVar(self: *AlgorithmJ) TypeVar {
        self.currentTypeVar += 1;
        return .{
            .n = self.currentTypeVar,
            .depth = self.depth,
            .subst = null,
        };
    }

    // Create a new type variable as a Type
    pub inline fn newVarT(self: *AlgorithmJ) Type {
        return .{ .data = .{ .typeVar = self.newVar() }, .rc = 1 };
    }

    // This function is used for checking if there is an infinite type
    // and also adjust the depth since typeVarA will be substituted with
    // something containing typeB, in order to not have two traversals
    // The depth needs to be the lower of both, since if one can't be generalised
    // the other also shouldn't
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
            .number => |numB| {
                return containsAndShiftDepth(typeVarA, numB.variable);
            },
            .composite => |composite| {
                var contains = false;
                for (composite.args.items) |arg| {
                    if (containsAndShiftDepth(typeVarA, arg)) {
                        contains = true;
                    }
                }
                return contains;
            },
        }
    }

    // Add a Substitution by modifying the type variable
    fn addSubstitution(typeVarA: *TypeVar, typeB: *Type) !void {
        var typeToSubstitute = typeB;
        var substituted = true;
        while (substituted) {
            substituted = false;
            switch (typeToSubstitute.data) {
                .typeVar => |typeVarB| {
                    if (typeVarB.subst) |sub| {
                        typeToSubstitute = sub;
                        substituted = true;
                    }
                },
                else => {},
            }
        }
        switch (typeToSubstitute.data) {
            .typeVar => |typeVarB| {
                if (typeVarB.n == typeVarA.n) {
                    return;
                }
            },
            else => {},
        }
        if (containsAndShiftDepth(typeVarA, typeB)) {
            return error.InfiniteType;
        }
        typeB.rc += 1;
        typeVarA.subst = typeB;
    }

    // Unify two types, but typeA must be <= typeB
    fn lessGeneralUnify(self: *AlgorithmJ, typeA: *Type, typeB: *Type) !void {
        switch (typeA.data) {
            .typeVar => |tV1| {
                switch (typeB.data) {
                    .typeVar => |*tV2| {
                        if (tV2.subst) |subst| {
                            try self.lessGeneralUnify(typeA, subst);
                        } else {
                            if (tV1.n != tV2.n) {
                                if (tV2.declaration) {
                                    return error.TooGeneral;
                                }
                                try addSubstitution(tV2, typeA);
                            }
                        }
                    },
                    else => {
                        return error.TooGeneral;
                    },
                }
            },
            .primitive => |prim1| {
                switch (typeB.data) {
                    .typeVar => |*tV2| {
                        if (tV2.subst) |subst| {
                            try self.lessGeneralUnify(typeA, subst);
                        } else {
                            if (tV2.declaration) {
                                return error.TooGeneral;
                            }
                            try addSubstitution(tV2, typeA);
                        }
                    },
                    .number => |number2| {
                        if (prim1 == .Float or prim1 == .Int) {
                            try self.lessGeneralUnify(typeA, number2.variable);
                        } else {
                            return error.CouldNotUnify;
                        }
                    },
                    .primitive => |prim2| {
                        if (prim1 != prim2) {
                            return error.CouldNotUnify;
                        }
                    },
                    .function => {
                        return error.CouldNotUnify;
                    },
                    .composite => {
                        return error.CouldNotUnify;
                    },
                }
            },
            .number => |*num1| {
                if (num1.variable.data.typeVar.subst) |subst1| {
                    switch (subst1.data) {
                        .typeVar => {
                            subst1.rc += 1;
                            num1.variable.deinit(self.allocator);
                            num1.variable = subst1;
                            try self.lessGeneralUnify(typeA, typeB);
                        },
                        .primitive => {
                            try self.lessGeneralUnify(subst1, typeB);
                        },
                        .number => {
                            try self.lessGeneralUnify(subst1, typeB);
                        },
                        else => {
                            return error.CouldNotUnify;
                        },
                    }
                } else {
                    switch (typeB.data) {
                        .typeVar => |*tV2| {
                            if (tV2.subst) |subst| {
                                try self.lessGeneralUnify(typeA, subst);
                            } else {
                                if (tV2.declaration) {
                                    return error.TooGeneral;
                                }
                                try addSubstitution(tV2, typeA);
                            }
                        },
                        .primitive => {
                            return error.TooGeneral;
                        },
                        .number => |num2| {
                            try self.lessGeneralUnify(num1.variable, num2.variable);
                        },
                        else => {
                            return error.CouldNotUnify;
                        },
                    }
                }
            },
            .function => |fun1| {
                switch (typeB.data) {
                    .typeVar => |*tV2| {
                        if (tV2.subst) |subst| {
                            try self.lessGeneralUnify(typeA, subst);
                        } else {
                            if (tV2.declaration) {
                                return error.TooGeneral;
                            }
                            try addSubstitution(tV2, typeA);
                        }
                    },
                    .function => |fun2| {
                        try self.lessGeneralUnify(fun1.from, fun2.from);
                        try self.lessGeneralUnify(fun1.to, fun2.to);
                    },
                    else => {
                        return error.CouldNotUnify;
                    },
                }
            },
            .composite => |composite1| {
                switch (typeB.data) {
                    .typeVar => |*tV2| {
                        if (tV2.subst) |subst| {
                            try self.lessGeneralUnify(typeA, subst);
                        } else {
                            if (tV2.declaration) {
                                return error.TooGeneral;
                            }
                            try addSubstitution(tV2, typeA);
                        }
                    },
                    .composite => |composite2| {
                        if (std.mem.eql(u8, composite1.name, composite2.name)) {
                            var i: usize = 0;
                            while (i < composite1.args.items.len) : (i += 1) {
                                try self.lessGeneralUnify(composite1.args.items[i], composite2.args.items[i]);
                            }
                        } else {
                            return error.CouldNotUnify;
                        }
                    },
                    else => {
                        return error.CouldNotUnify;
                    },
                }
            },
        }
    }

    // Unify typeA and typeB creating substitutions as needed
    pub fn unify(self: *AlgorithmJ, typeA: *Type, typeB: *Type) !void {
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
                    .number => |numB| {
                        switch (primitiveA) {
                            .Int, .Float => {
                                try self.unify(numB.variable, typeA);
                            },
                            .Bool, .Char => {
                                return error.CouldNotUnify;
                            },
                        }
                    },
                    .composite => {
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
                    .number => {
                        return error.CouldNotUnify;
                    },
                    .composite => {
                        return error.CouldNotUnify;
                    },
                }
            },
            .number => |numA| {
                switch (typeB.data) {
                    .primitive => |prim| {
                        switch (prim) {
                            .Int, .Float => {
                                try self.unify(numA.variable, typeB);
                            },
                            .Bool, .Char => {
                                return error.CouldNotUnify;
                            },
                        }
                    },
                    .function => {
                        return error.CouldNotUnify;
                    },
                    .typeVar => {
                        try self.unify(typeB, typeA);
                    },
                    .number => |numB| {
                        try self.unify(numA.variable, numB.variable);
                    },
                    .composite => {
                        return error.CouldNotUnify;
                    },
                }
            },
            .composite => |composite1| {
                switch (typeB.data) {
                    .typeVar => {
                        try self.unify(typeB, typeA);
                    },
                    .composite => |composite2| {
                        if (std.mem.eql(u8, composite1.name, composite2.name)) {
                            var i: usize = 0;
                            while (i < composite1.args.items.len) : (i += 1) {
                                try self.unify(composite1.args.items[i], composite2.args.items[i]);
                            }
                        } else {
                            return error.CouldNotUnify;
                        }
                    },
                    else => {
                        return error.CouldNotUnify;
                    },
                }
            },
        }
    }

    // Recursively copy the Type while replacing type variables
    fn copyAndReplace(self: *AlgorithmJ, inputType: *Type, replacementMap: *const std.AutoHashMap(usize, *Type), copyMap: *std.AutoHashMap(*Type, *Type)) !*Type {
        if (copyMap.get(inputType)) |copied| {
            copied.rc += 1;
            return copied;
        }
        var result: *Type = undefined;
        switch (inputType.data) {
            .typeVar => |typeVar| {
                if (typeVar.subst) |subst| {
                    const t = try Type.init(self.allocator);
                    errdefer self.allocator.destroy(t);
                    t.data = .{ .typeVar = .{
                        .n = typeVar.n,
                        .depth = self.depth,
                        .subst = try self.copyAndReplace(subst, replacementMap, copyMap),
                    } };
                    result = t;
                } else {
                    if (replacementMap.get(typeVar.n)) |newTypeVar| {
                        newTypeVar.rc += 1;
                        result = newTypeVar;
                    } else {
                        inputType.rc += 1;
                        result = inputType;
                    }
                }
            },
            .primitive => |primitive| {
                const t = try Type.init(self.allocator);
                t.data = .{ .primitive = primitive };
                result = t;
            },
            .number => |num| {
                const t = try Type.init(self.allocator);
                errdefer self.allocator.destroy(t);
                const varCopied = try self.copyAndReplace(num.variable, replacementMap, copyMap);
                t.data = .{ .number = .{
                    .variable = varCopied,
                } };
                result = t;
            },
            .function => |function| {
                const t = try Type.init(self.allocator);
                errdefer self.allocator.destroy(t);
                const fromCopied = try self.copyAndReplace(function.from, replacementMap, copyMap);
                errdefer fromCopied.deinit(self.allocator);
                t.data = .{ .function = .{
                    .from = fromCopied,
                    .to = try self.copyAndReplace(function.to, replacementMap, copyMap),
                } };
                result = t;
            },
            .composite => |composite| {
                const t = try Type.init(self.allocator);
                errdefer self.allocator.destroy(t);
                var copiedArgs = std.ArrayList(*Type).init(self.allocator);
                errdefer {
                    for (copiedArgs.items) |arg| {
                        arg.deinit(self.allocator);
                    }
                    copiedArgs.deinit();
                }
                for (composite.args.items) |arg| {
                    try copiedArgs.append(try self.copyAndReplace(
                        arg,
                        replacementMap,
                        copyMap,
                    ));
                }
                t.data = .{ .composite = .{
                    .name = composite.name,
                    .args = copiedArgs,
                } };
                result = t;
            },
        }
        errdefer result.deinit(self.allocator);
        try copyMap.put(inputType, result);
        result.rc += 1;
        return result;
    }

    // Create a replacement map and apply it
    pub fn instantiate(self: *AlgorithmJ, forall: *Forall) !*Type {
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
        var copyMap = std.AutoHashMap(*Type, *Type).init(self.allocator);
        defer {
            var copyMapIter = copyMap.valueIterator();
            while (copyMapIter.next()) |copyType| {
                copyType.*.deinit(self.allocator);
            }
            copyMap.deinit();
        }
        return try self.copyAndReplace(forall.type, &instantiatedVars, &copyMap);
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
            .number => |num| {
                try self.findFreeVars(num.variable, minDepth, currentFreeVars);
            },
            .composite => |composite| {
                for (composite.args.items) |arg| {
                    try self.findFreeVars(arg, minDepth, currentFreeVars);
                }
            },
        }
    }

    // Find all free vars, then put them in a forall
    pub fn generalise(self: *AlgorithmJ, t: *Type, minDepth: usize) !*TypeScheme {
        var freeVars = TypeVarSet.init(self.allocator);
        try self.findFreeVars(t, minDepth, &freeVars);
        errdefer freeVars.deinit();
        const typeScheme = try self.allocator.create(TypeScheme);
        if (freeVars.count() == 0) {
            freeVars.deinit();
            typeScheme.* = .{ .type = t };
        } else {
            typeScheme.* = .{ .forall = .{
                .typeVars = freeVars,
                .type = t,
            } };
        }
        return typeScheme;
    }

    fn getLetVarType(self: *AlgorithmJ, name: token.Token, value: *AST, annotation: ?TypeAnnotation) anyerror!*TypeScheme {
        self.depth += 1;
        var typeOfVarTypeVar: *Type = undefined;
        var typeOfVarScheme: *TypeScheme = undefined;
        switch (value.*) {
            .lambda => {
                typeOfVarTypeVar = try Type.init(self.allocator);
                typeOfVarTypeVar.* = self.newVarT();
                typeOfVarScheme = self.allocator.create(TypeScheme) catch |err| {
                    typeOfVarTypeVar.deinit(self.allocator);
                    return err;
                };
                typeOfVarScheme.* = .{ .type = typeOfVarTypeVar };
                {
                    errdefer deinitScheme(typeOfVarScheme, self.allocator);
                    try self.globalTypes.put(name.lexeme, typeOfVarScheme);
                }
            },
            else => {},
        }
        const typeOfVar = try self.run(value);
        switch (value.*) {
            .lambda => {
                errdefer typeOfVar.deinit(self.allocator);
                self.unify(typeOfVarTypeVar, typeOfVar) catch |err| switch (err) {
                    error.CouldNotUnify => {
                        try self.errors.recursionTwoTypes(value, name.lexeme, typeOfVarTypeVar, typeOfVar);
                        return err;
                    },
                    error.InfiniteType => {
                        try self.errors.recursionInfiniteType(value, name.lexeme, typeOfVarTypeVar, typeOfVar);
                        return err;
                    },
                };
                deinitScheme(typeOfVarScheme, self.allocator);
            },
            else => {},
        }
        if (annotation) |typeAnnotation| {
            errdefer typeOfVar.deinit(self.allocator);
            self.lessGeneralUnify(typeAnnotation.type, typeOfVar) catch |err| switch (err) {
                error.TooGeneral => {
                    try self.errors.tooGeneralVariableType(value, typeOfVar, typeAnnotation.region);
                    return err;
                },
                error.CouldNotUnify => {
                    try self.errors.typeComparison(
                        computeBoundaries(value),
                        typeOfVar,
                        typeAnnotation.type,
                        "should be the same as this",
                        "this type is annotated.",
                        typeAnnotation.region,
                    );
                    return err;
                },
                else => {
                    return err;
                },
            };
        }
        self.depth -= 1;
        const generalised = self.generalise(typeOfVar, self.depth + 1) catch |err| {
            typeOfVar.deinit(self.allocator);
            return err;
        };
        errdefer typeOfVar.deinit(self.allocator);
        return generalised;
    }

    // The main algorithm as described in the paper
    pub fn run(self: *AlgorithmJ, ast: *AST) !*Type {
        var t = try Type.init(self.allocator);
        switch (ast.*) {
            .identifier => |*id| {
                self.allocator.destroy(t);
                if (self.globalTypes.get(id.token.lexeme)) |typeOfIdScheme| {
                    switch (typeOfIdScheme.*) {
                        .type => |typeOfId| {
                            typeOfId.rc += 1;
                            t = typeOfId;
                        },
                        .forall => |*forall| {
                            t = try self.instantiate(forall);
                        },
                    }
                    t.rc += 1;
                    id.idType = t;
                } else {
                    try self.errors.errorAt(id.token.start, id.token.end, "Unknown identifier", .{});
                    return error.UnknownIdentifier;
                }
            },
            .intConstant => {
                t.data = .{ .primitive = .Int };
            },
            .floatConstant => {
                t.data = .{ .primitive = .Float };
            },
            .boolConstant => {
                t.data = .{ .primitive = .Bool };
            },
            .charConstant => {
                t.data = .{ .primitive = .Char };
            },
            .prefixOp => |*prefixOp| {
                const char = prefixOp.token.lexeme[0];
                if (char == '-') {
                    const tV = try Type.init(self.allocator);
                    tV.* = self.newVarT();
                    t.data = .{ .number = .{ .variable = tV } };
                } else {
                    t.data = .{ .primitive = .Bool };
                }
                errdefer t.deinit(self.allocator);
                const typeOfExpr = try self.run(prefixOp.expr);
                defer typeOfExpr.deinit(self.allocator);
                self.unify(typeOfExpr, t) catch |err| switch (err) {
                    error.CouldNotUnify => {
                        try self.errors.typeComparison(
                            computeBoundaries(prefixOp.expr),
                            typeOfExpr,
                            t,
                            "should be the same as the type of this",
                            "it is given as an argument to a prefix operator:",
                            .{ .start = prefixOp.token.start, .end = prefixOp.token.end },
                        );
                        return err;
                    },
                    else => {
                        return err;
                    },
                };
                prefixOp.argType = t;
                t.rc += 1;
            },
            .operator => |*op| {
                errdefer self.allocator.destroy(t);
                const leftType = try self.run(op.left);
                defer leftType.deinit(self.allocator);
                const rightType = try self.run(op.right);
                errdefer rightType.deinit(self.allocator);
                op.argType = rightType;
                rightType.rc += 1;
                if (op.token.lexeme[0] != ';') {
                    self.unify(leftType, rightType) catch |err| switch (err) {
                        error.CouldNotUnify => {
                            try self.errors.typeMismatch(
                                op.left,
                                op.right,
                                leftType,
                                rightType,
                                "they are arguments to the same operator",
                            );
                            return err;
                        },
                        else => {
                            return err;
                        },
                    };
                }
                if (op.token.lexeme[0] == ';') {
                    self.allocator.destroy(t);
                    return rightType;
                } else if (op.token.lexeme[0] == 'o' or op.token.lexeme[0] == 'a') {
                    // Or and and are boolean operators
                    const boolType = try Type.init(self.allocator);
                    defer boolType.deinit(self.allocator);
                    boolType.data = .{ .primitive = .Bool };
                    self.unify(leftType, boolType) catch |err| switch (err) {
                        error.CouldNotUnify => {
                            try self.errors.typeComparison(
                                computeBoundaries(op.left),
                                leftType,
                                boolType,
                                "should be the same as this type",
                                "it is given as an argument to a boolean operator:",
                                .{ .start = op.token.start, .end = op.token.end },
                            );
                            return err;
                        },
                        else => {
                            return err;
                        },
                    };
                } else if (op.token.lexeme[0] == '^') {
                    const floatType = try Type.init(self.allocator);
                    defer floatType.deinit(self.allocator);
                    floatType.data = .{ .primitive = .Float };
                    self.unify(leftType, floatType) catch |err| switch (err) {
                        error.CouldNotUnify => {
                            try self.errors.typeComparison(
                                computeBoundaries(op.left),
                                leftType,
                                floatType,
                                "should be the same as this type",
                                "it is given as an argument to exponentiation:",
                                .{ .start = op.token.start, .end = op.token.end },
                            );
                            return err;
                        },
                        else => {
                            return err;
                        },
                    };
                } else if (op.token.lexeme[0] != '=' and op.token.lexeme[0] != '!') {
                    const tV = try Type.init(self.allocator);
                    tV.* = self.newVarT();
                    const tNumber = Type.init(self.allocator) catch |err| {
                        tV.deinit(self.allocator);
                        return err;
                    };
                    defer tNumber.deinit(self.allocator);
                    tNumber.data = .{
                        .number = .{ .variable = tV },
                    };
                    self.unify(leftType, tNumber) catch |err| switch (err) {
                        error.CouldNotUnify => {
                            try self.errors.typeComparison(
                                computeBoundaries(op.left),
                                leftType,
                                tNumber,
                                "should be the same as this type",
                                "it is given as an argument to a numeric operator:",
                                .{ .start = op.token.start, .end = op.token.end },
                            );
                            return err;
                        },
                        else => {
                            return err;
                        },
                    };
                }
                switch (op.token.lexeme[0]) {
                    // 'o' and 'a' are the start of or and and
                    '+', '-', '*', '/', 'o', 'a', '^' => {
                        self.allocator.destroy(t);
                        return rightType;
                    },
                    '<', '>', '=', '!' => {
                        rightType.deinit(self.allocator);
                        t.data = .{ .primitive = .Bool };
                    },
                    else => {},
                }
            },
            .ifExpr => |*ifExpr| {
                self.allocator.destroy(t);
                const boolT = try Type.init(self.allocator);
                boolT.data = .{
                    .primitive = .Bool,
                };
                defer boolT.deinit(self.allocator);
                const typeOfPredicate = try self.run(ifExpr.predicate);
                defer typeOfPredicate.deinit(self.allocator);
                self.unify(boolT, typeOfPredicate) catch |err| switch (err) {
                    error.CouldNotUnify => {
                        try self.errors.typeComparison(
                            computeBoundaries(ifExpr.predicate),
                            typeOfPredicate,
                            boolT,
                            "should have this type: ",
                            "it is an if predicate",
                            .{ .start = ifExpr.start, .end = ifExpr.start + 2 },
                        );
                        return err;
                    },
                    else => {
                        return err;
                    },
                };
                const typeOfThen = try self.run(ifExpr.thenExpr);
                defer typeOfThen.deinit(self.allocator);
                const typeOfElse = try self.run(ifExpr.elseExpr);
                errdefer typeOfElse.deinit(self.allocator);
                self.unify(typeOfThen, typeOfElse) catch |err| switch (err) {
                    error.CouldNotUnify => {
                        try self.errors.typeMismatch(
                            ifExpr.thenExpr,
                            ifExpr.elseExpr,
                            typeOfThen,
                            typeOfElse,
                            "they are the two branches of an if expression",
                        );
                        return err;
                    },
                    error.InfiniteType => {
                        try self.errors.typeComparison(
                            computeBoundaries(ifExpr.thenExpr),
                            typeOfThen,
                            typeOfElse,
                            "leads to an infinite type\nwhen combined with the type of this",
                            "they are the two branches of an if expression",
                            computeBoundaries(ifExpr.elseExpr),
                        );
                        return err;
                    },
                };
                typeOfElse.rc += 1;
                ifExpr.resultType = typeOfElse;
                return typeOfElse;
            },
            .call => |*call| {
                t.* = self.newVarT();
                errdefer t.deinit(self.allocator);
                const t1 = try self.run(call.function);
                defer t1.deinit(self.allocator);
                const t2 = try self.run(call.arg);
                defer t2.deinit(self.allocator);
                const fType = try Type.init(self.allocator);
                t2.rc += 1;
                t.rc += 1;
                fType.data = .{ .function = .{
                    .from = t2,
                    .to = t,
                } };
                defer fType.deinit(self.allocator);
                fType.rc += 1;
                call.functionType = fType;
                self.unify(t1, fType) catch |err| switch (err) {
                    error.CouldNotUnify => {
                        try self.errors.typeComparison(
                            computeBoundaries(call.function),
                            t1,
                            fType,
                            "should be the same as the type of this",
                            "this is given as an argument to it:",
                            computeBoundaries(call.arg),
                        );
                        return err;
                    },
                    error.InfiniteType => {
                        try self.errors.typeComparison(
                            computeBoundaries(call.function),
                            t1,
                            fType,
                            "leads to an infinite type\nwhen combined with the type of this",
                            "this is given as an argument to it:",
                            computeBoundaries(call.arg),
                        );
                        return err;
                    },
                };
            },
            .lambda => |*lambda| {
                errdefer self.allocator.destroy(t);
                const newTypeVar = try Type.init(self.allocator);
                newTypeVar.* = self.newVarT();
                errdefer newTypeVar.deinit(self.allocator);
                var typeScheme: *TypeScheme = undefined;
                {
                    errdefer newTypeVar.deinit(self.allocator);
                    typeScheme = try self.allocator.create(TypeScheme);
                }
                typeScheme.* = .{ .type = newTypeVar };
                newTypeVar.rc += 1;
                if (self.globalTypes.contains(lambda.argname.lexeme)) {
                    deinitScheme(typeScheme, self.allocator);
                    try self.errors.errorAt(
                        lambda.argname.start,
                        lambda.argname.end,
                        "This name shadows another variable.",
                        .{},
                    );
                    return error.CouldNotUnify;
                }
                {
                    errdefer deinitScheme(typeScheme, self.allocator);
                    try self.globalTypes.put(lambda.argname.lexeme, typeScheme);
                }
                const returnType = try self.run(lambda.expr);
                errdefer returnType.deinit(self.allocator);
                _ = self.globalTypes.remove(lambda.argname.lexeme);
                deinitScheme(typeScheme, self.allocator);
                t.data = .{ .function = .{
                    .from = newTypeVar,
                    .to = returnType,
                } };
                if (lambda.argType) |argType| {
                    self.lessGeneralUnify(argType.type, newTypeVar) catch |err| switch (err) {
                        error.TooGeneral => {
                            try self.errors.tooGeneralArgumentType(ast, newTypeVar);
                            return err;
                        },
                        error.CouldNotUnify => {
                            try self.errors.typeComparison(
                                .{ .start = lambda.argname.start, .end = lambda.argname.end },
                                newTypeVar,
                                argType.type,
                                "should be the same as this",
                                "this type is annotated.",
                                argType.region,
                            );
                            return err;
                        },
                        else => {
                            return err;
                        },
                    };
                }
                lambda.type = t;
                t.rc += 1;
            },
            .let => |*let| {
                self.allocator.destroy(t);
                if (self.globalTypes.contains(let.name.lexeme)) {
                    try self.errors.errorAt(
                        let.name.start,
                        let.name.end,
                        "This name shadows another variable.",
                        .{},
                    );
                    return error.CouldNotUnify;
                }
                const generalised = try self.getLetVarType(let.name, let.be, let.type);
                self.globalTypes.put(let.name.lexeme, generalised) catch |err| {
                    deinitScheme(generalised, self.allocator);
                    return err;
                };
                const typeOfExpr = try self.run(let.in);
                errdefer typeOfExpr.deinit(self.allocator);
                _ = self.globalTypes.remove(let.name.lexeme);
                let.actualType = generalised;
                return typeOfExpr;
            },
            .case => |*case| {
                self.allocator.destroy(t);
                const typeOfValue = try self.run(case.value);
                defer typeOfValue.deinit(self.allocator);
                var type_: ?[]const u8 = null;
                for (case.patterns.items) |pattern| {
                    if (self.constructorToType.get(pattern.name.lexeme)) |constructorType| {
                        if (type_ != null and !std.mem.eql(u8, type_.?, constructorType)) {
                            try self.errors.errorAt(
                                pattern.name.start,
                                pattern.name.end,
                                "This constructor belongs to the type '{s}',\nbut the previous patterns belong to the type '{s}'.",
                                .{ constructorType, type_.? },
                            );
                            return error.CouldNotUnify;
                        }
                        type_ = constructorType;
                    } else {
                        try self.errors.errorAt(
                            pattern.name.start,
                            pattern.name.end,
                            "The constructor '{s}' doesn't exist.",
                            .{pattern.name.lexeme},
                        );
                        return error.UnknownIdentifier;
                    }
                }
                var composite = self.composite.getPtr(type_.?).?;

                var instantiatedVars = std.AutoHashMap(usize, *Type).init(self.allocator);

                const args = composite.compositeType.?.data.composite.args;
                {
                    errdefer {
                        var instIterator = instantiatedVars.iterator();
                        while (instIterator.next()) |entry| {
                            self.allocator.destroy(entry.value_ptr.*);
                        }
                    }
                    for (args.items) |arg| {
                        const varT = try self.allocator.create(Type);
                        errdefer self.allocator.destroy(varT);
                        varT.* = self.newVarT();
                        try instantiatedVars.put(arg.data.typeVar.n, varT);
                    }
                }
                defer {
                    var instIterator = instantiatedVars.iterator();
                    while (instIterator.next()) |entry| {
                        entry.value_ptr.*.deinit(self.allocator);
                    }
                    instantiatedVars.deinit();
                }

                var copyMap = std.AutoHashMap(*Type, *Type).init(self.allocator);
                defer {
                    var copyMapIter = copyMap.iterator();
                    while (copyMapIter.next()) |copyType| {
                        copyType.value_ptr.*.deinit(self.allocator);
                    }
                    copyMap.deinit();
                }

                const copiedCompositeType = try self.copyAndReplace(composite.compositeType.?, &instantiatedVars, &copyMap);
                defer case.valueType = copiedCompositeType;
                self.unify(typeOfValue, copiedCompositeType) catch |err| switch (err) {
                    error.CouldNotUnify => {
                        try self.errors.typeComparison(
                            computeBoundaries(case.value),
                            typeOfValue,
                            copiedCompositeType,
                            "should be the same as this",
                            "the value is matched with this constructor.",
                            computeBoundaries(case.bodies.items[0]),
                        );
                        return err;
                    },
                    error.InfiniteType => {
                        try self.errors.typeComparison(
                            computeBoundaries(case.value),
                            typeOfValue,
                            copiedCompositeType,
                            "leads to an infinite type\nwhen combined with the type of this",
                            "the value is matched with this constructor.",
                            computeBoundaries(case.bodies.items[0]),
                        );
                        return err;
                    },
                };

                for (composite.constructors.?.values()) |*constructor| {
                    constructor.* = false;
                }

                for (case.patterns.items) |pattern| {
                    if (composite.constructors.?.get(pattern.name.lexeme).?) {
                        try self.errors.errorAt(
                            pattern.name.start,
                            pattern.name.end,
                            "This constructor has already been matched.",
                            .{},
                        );
                        return error.CouldNotUnify;
                    } else {
                        composite.constructors.?.getPtr(pattern.name.lexeme).?.* = true;
                    }
                }

                for (composite.constructors.?.keys(), composite.constructors.?.values()) |key, value| {
                    if (!value) {
                        try self.errors.errorAt(
                            case.start,
                            computeBoundaries(ast).end,
                            "Constructor {s} is not matched.",
                            .{key},
                        );
                        return error.NonExhaustiveMatch;
                    }
                }

                const resultType = try Type.init(self.allocator);
                resultType.* = self.newVarT();
                errdefer resultType.deinit(self.allocator);
                for (case.patterns.items, case.bodies.items) |*pattern, body| {
                    const constructorType = switch (self.globalTypes.get(pattern.name.lexeme).?.*) {
                        .forall => |forall| forall.type,
                        .type => |exactType| exactType,
                    };
                    const substitutedType = try self.copyAndReplace(constructorType, &instantiatedVars, &copyMap);
                    defer substitutedType.deinit(self.allocator);
                    var constrType = substitutedType;
                    var matchedTypes = try std.ArrayList(*Type).initCapacity(
                        self.allocator,
                        pattern.values.items.len,
                    );
                    errdefer matchedTypes.deinit();
                    errdefer for (matchedTypes.items) |matchedType| {
                        matchedType.deinit(self.allocator);
                    };
                    for (pattern.values.items) |value| {
                        switch (constrType.data) {
                            .composite => {
                                try self.errors.errorAt(
                                    pattern.name.start,
                                    pattern.name.end,
                                    "Too many arguments given to the constructor.",
                                    .{},
                                );
                            },
                            .function => |func| {
                                if (self.globalTypes.contains(value.lexeme)) {
                                    try self.errors.errorAt(
                                        value.start,
                                        value.end,
                                        "This name shadows another variable.",
                                        .{},
                                    );
                                    return error.CouldNotUnify;
                                }
                                const valueScheme = try self.allocator.create(TypeScheme);
                                errdefer self.allocator.destroy(valueScheme);

                                valueScheme.* = .{ .type = func.from };
                                func.from.rc += 1;
                                try self.globalTypes.put(value.lexeme, valueScheme);
                                func.from.rc += 1;
                                try matchedTypes.append(func.from);
                                constrType = func.to;
                            },
                            else => {
                                unreachable;
                            },
                        }
                    }
                    switch (constrType.data) {
                        .composite => {},
                        else => {
                            try self.errors.errorAt(
                                pattern.name.start,
                                pattern.name.end,
                                "Too few arguments given to the constructor.",
                                .{},
                            );
                        },
                    }
                    const bodyType = try self.run(body);
                    defer bodyType.deinit(self.allocator);
                    for (pattern.values.items) |value| {
                        deinitScheme(self.globalTypes.get(value.lexeme).?, self.allocator);
                        _ = self.globalTypes.remove(value.lexeme);
                    }
                    self.unify(resultType, bodyType) catch |err| switch (err) {
                        error.CouldNotUnify => {
                            try self.errors.typeComparison(
                                computeBoundaries(body),
                                bodyType,
                                resultType,
                                "should be the same as this",
                                "they are from the same pattern match.",
                                computeBoundaries(case.bodies.items[0]),
                            );
                            return err;
                        },
                        error.InfiniteType => {
                            try self.errors.typeComparison(
                                computeBoundaries(body),
                                bodyType,
                                resultType,
                                "leads to an infinite type\nwhen combined with the type of this",
                                "they are from the same pattern match:",
                                computeBoundaries(case.bodies.items[0]),
                            );
                            return err;
                        },
                    };
                    pattern.types = matchedTypes;
                }
                resultType.rc += 1;
                case.resultType = resultType;
                return resultType;
            },
            .list => |list| {
                t.* = self.newVarT();
                errdefer t.deinit(self.allocator);
                for (list.values.items, 0..) |value, i| {
                    const typeOfVal = try self.run(value);
                    defer typeOfVal.deinit(self.allocator);
                    self.unify(t, typeOfVal) catch |err| switch (err) {
                        error.CouldNotUnify => {
                            try self.errors.typeComparison(
                                computeBoundaries(value),
                                typeOfVal,
                                t,
                                "should be the same as this",
                                "it is the type of the previous elements.",
                                .{
                                    .start = computeBoundaries(list.values.items[0]).start,
                                    .end = computeBoundaries(list.values.items[i - 1]).end,
                                },
                            );
                            return err;
                        },
                        error.InfiniteType => {
                            try self.errors.typeComparison(
                                computeBoundaries(value),
                                typeOfVal,
                                t,
                                "leads to an infinite type\nwhen combined with the type of this",
                                "it is the type of the previous elements.",
                                .{
                                    .start = computeBoundaries(list.values.items[0]).start,
                                    .end = computeBoundaries(list.values.items[i - 1]).end,
                                },
                            );
                            return err;
                        },
                    };
                }
                var listArgs = std.ArrayList(*Type).init(self.allocator);
                errdefer listArgs.deinit();
                try listArgs.append(t);
                const listType = try Type.init(self.allocator);
                listType.data = .{ .composite = .{
                    .name = "List",
                    .args = listArgs,
                } };
                return listType;
            },
            .lambdaMult => {},
            .callMult => {},
        }
        return t;
    }

    pub fn checkStatement(self: *AlgorithmJ, statement: *Statement) !void {
        switch (statement.*) {
            .let => |*let| {
                if (self.globalTypes.contains(let.name.lexeme)) {
                    try self.errors.errorAt(let.name.start, let.name.end, "The name '{s}' is already used.", .{let.name.lexeme});
                    return error.CouldNotUnify;
                }
                const t = try self.getLetVarType(let.name, let.be, let.annotation);
                let.actualType = t;
                try self.globalTypes.put(let.name.lexeme, t);
            },
            .type => |*typeDecl| {
                const prevDepth = self.depth;
                self.depth = std.math.maxInt(usize);
                typeDecl.constructorTypeSchemes = .init(self.allocator);
                for (typeDecl.constructors.items) |constructor| {
                    if (self.globalTypes.contains(constructor.name.lexeme)) {
                        try self.errors.errorAt(
                            constructor.name.start,
                            constructor.name.end,
                            "The name '{s}' is already used.",
                            .{constructor.name.lexeme},
                        );
                    }
                    var currentType = typeDecl.compositeType;
                    currentType.rc += 1;
                    errdefer currentType.deinit(self.allocator);
                    var i: usize = constructor.args.items.len;
                    while (i > 0) : (i -= 1) {
                        const functionType = try Type.init(self.allocator);
                        constructor.args.items[i - 1].rc += 1;
                        functionType.data = .{ .function = .{
                            .from = constructor.args.items[i - 1],
                            .to = currentType,
                        } };
                        currentType = functionType;
                    }
                    const constructorType = try self.allocator.create(TypeScheme);
                    errdefer self.allocator.destroy(constructorType);
                    if (typeDecl.typeArgs.items.len != 0) {
                        var typeVarSet: TypeVarSet = .init(self.allocator);
                        errdefer typeVarSet.deinit();
                        for (typeDecl.typeArgs.items) |t| {
                            try typeVarSet.put(t.data.typeVar.n, undefined);
                        }

                        constructorType.* = .{ .forall = .{
                            .typeVars = typeVarSet,
                            .type = currentType,
                        } };
                    } else {
                        constructorType.* = .{ .type = currentType };
                    }
                    errdefer constructorType.forall.typeVars.deinit();
                    try self.constructorToType.put(constructor.name.lexeme, typeDecl.name.lexeme);
                    try typeDecl.constructorTypeSchemes.?.append(constructorType);
                    try self.globalTypes.put(constructor.name.lexeme, constructorType);
                }
                self.depth = prevDepth;
            },
        }
    }
};
