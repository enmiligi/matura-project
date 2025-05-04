const std = @import("std");
const token = @import("./token.zig");
const AST = @import("./ast.zig").AST;
const Statement = @import("./ast.zig").Statement;
const Value = @import("./value.zig").Value;
const object = @import("./object.zig");
const errors = @import("./errors.zig");

const nullToken = token.Token{ .start = 0, .end = 0, .lexeme = "", .type = .Identifier };

const EvalError = error{
    UnknownIdentifier,

    // This should never happen because of the type inference/checking
    UnexpectedType,
} || std.mem.Allocator.Error;

pub const Env = struct {
    contents: *std.StringHashMap(Value),
    next: ?*Env,
};

pub const Interpreter = struct {
    objects: object.Objects,
    // This is a stack of Values preserved while collecting garbage
    preserveValues: *std.ArrayList(Value),
    currentEnv: *Env,
    allocator: std.mem.Allocator,

    argNameMap: std.StringHashMap(std.ArrayList(token.Token)),

    pub fn init(
        allocator: std.mem.Allocator,
        initialEnv: *std.StringHashMap(Value),
    ) !Interpreter {
        const preserveValues = try allocator.create(std.ArrayList(Value));
        errdefer allocator.destroy(preserveValues);
        preserveValues.* = .init(allocator);
        const env = try allocator.create(Env);
        env.* = .{
            .contents = initialEnv,
            .next = null,
        };
        return .{
            .objects = object.Objects.init(allocator, preserveValues, env),
            .preserveValues = preserveValues,
            .currentEnv = env,
            .allocator = allocator,
            .argNameMap = .init(allocator),
        };
    }

    fn deinitEnvs(self: *Interpreter) void {
        while (self.currentEnv.next) |next| {
            self.currentEnv.contents.deinit();
            self.allocator.destroy(self.currentEnv);
            self.currentEnv = next;
        }
        self.currentEnv.contents.deinit();
        self.allocator.destroy(self.currentEnv);
    }

    pub fn deinit(self: *Interpreter) void {
        self.preserveValues.deinit();
        self.allocator.destroy(self.preserveValues);
        self.objects.deinit();
        self.deinitEnvs();
        var argNameIterator = self.argNameMap.iterator();
        while (argNameIterator.next()) |argNames| {
            for (argNames.value_ptr.items) |argName| {
                self.allocator.free(argName.lexeme);
            }
            argNames.value_ptr.deinit();
        }
        self.argNameMap.deinit();
    }

    fn pushValue(self: *Interpreter, val: Value) !void {
        try self.preserveValues.append(val);
    }

    fn popValue(self: *Interpreter) void {
        _ = self.preserveValues.pop();
    }

    fn pushEnv(self: *Interpreter, map: *std.StringHashMap(Value)) !void {
        const env = try self.allocator.create(Env);
        env.* = .{
            .contents = map,
            .next = self.currentEnv,
        };
        self.currentEnv = env;
        self.objects.currentEnv = env;
    }

    fn popEnv(self: *Interpreter) void {
        const next = self.currentEnv.next;
        self.allocator.destroy(self.currentEnv);
        self.currentEnv = next.?;
        self.objects.currentEnv = next.?;
    }

    pub fn lookup(self: *Interpreter, name: []const u8) ?Value {
        var env = self.currentEnv;
        while (!env.contents.contains(name)) {
            if (env.next) |nextEnv| {
                env = nextEnv;
            }
            return null;
        }
        return env.contents.get(name);
    }

    inline fn set(self: *Interpreter, name: []const u8, val: Value) !void {
        try self.currentEnv.contents.put(name, val);
    }

    inline fn remove(self: *Interpreter, name: []const u8) void {
        _ = self.currentEnv.contents.remove(name);
    }

    // When numbers are added, subtracted, multiplied or divided
    fn evalNumberOp(op: token.Token, left: Value, right: Value) !Value {
        switch (left) {
            .int => |int1| {
                switch (right) {
                    .int => |int2| {
                        const res = switch (op.lexeme[0]) {
                            '+' => int1 + int2,
                            '-' => int1 - int2,
                            '*' => int1 * int2,
                            '/' => @divFloor(int1, int2),
                            else => undefined,
                        };
                        return .{ .int = res };
                    },
                    else => {
                        return error.UnexpectedType;
                    },
                }
            },
            .float => |float1| {
                switch (right) {
                    .float => |float2| {
                        const res = switch (op.lexeme[0]) {
                            '+' => float1 + float2,
                            '-' => float1 - float2,
                            '*' => float1 * float2,
                            '/' => float1 / float2,
                            else => undefined,
                        };
                        return .{ .float = res };
                    },
                    else => {
                        return error.UnexpectedType;
                    },
                }
            },
            else => {
                return error.UnexpectedType;
            },
        }
    }

    // Two values are compared
    fn evalComp(op: token.Token, left: Value, right: Value) !Value {
        switch (left) {
            .int => |int1| {
                switch (right) {
                    .int => |int2| {
                        const res = switch (op.lexeme[0]) {
                            '<' => int1 < int2,
                            '>' => int1 > int2,
                            '=' => int1 == int2,
                            '!' => int1 != int2,
                            else => undefined,
                        };
                        return .{ .bool = res };
                    },
                    else => {
                        return error.UnexpectedType;
                    },
                }
            },
            .float => |float1| {
                switch (right) {
                    .float => |float2| {
                        const res = switch (op.lexeme[0]) {
                            '<' => float1 < float2,
                            '>' => float1 > float2,
                            '=' => float1 == float2,
                            '!' => float1 != float2,
                            else => undefined,
                        };
                        return .{ .bool = res };
                    },
                    else => {
                        return error.UnexpectedType;
                    },
                }
            },
            .bool => |bool1| {
                switch (right) {
                    .bool => |bool2| {
                        const res = switch (op.lexeme[0]) {
                            '=' => bool1 == bool2,
                            '!' => bool1 != bool2,
                            else => undefined,
                        };
                        return .{ .bool = res };
                    },
                    else => {
                        return error.UnexpectedType;
                    },
                }
            },
            .object => |object1| {
                switch (object1.content) {
                    .recurse => |rec1| {
                        if (rec1) |rec1Value| {
                            return evalComp(op, rec1Value, right);
                        } else {
                            return error.UnknownIdentifier;
                        }
                    },
                    .closure => {
                        switch (right) {
                            .object => |object2| {
                                switch (object2.content) {
                                    .recurse => |rec2| {
                                        if (rec2) |rec2Value| {
                                            return evalComp(op, left, rec2Value);
                                        } else {
                                            return error.UnknownIdentifier;
                                        }
                                    },
                                    .closure => {
                                        const res = switch (op.lexeme[0]) {
                                            '=' => object1 == object2,
                                            '!' => object1 != object2,
                                            else => undefined,
                                        };
                                        return .{ .bool = res };
                                    },
                                    else => {},
                                }
                            },
                            else => {},
                        }
                    },
                    .multiArgClosure => {
                        switch (right) {
                            .object => |object2| {
                                switch (object2.content) {
                                    .recurse => |rec2| {
                                        if (rec2) |rec2Value| {
                                            return evalComp(op, left, rec2Value);
                                        } else {
                                            return error.UnknownIdentifier;
                                        }
                                    },
                                    .multiArgClosure => {
                                        const res = switch (op.lexeme[0]) {
                                            '=' => object1 == object2,
                                            '!' => object1 != object2,
                                            else => undefined,
                                        };
                                        return .{ .bool = res };
                                    },
                                    else => {},
                                }
                            },
                            else => {},
                        }
                    },
                    .construct => {
                        switch (right) {
                            .object => |object2| {
                                switch (object2.content) {
                                    .recurse => |rec2| {
                                        if (rec2) |rec2Value| {
                                            return evalComp(op, left, rec2Value);
                                        } else {
                                            return error.UnknownIdentifier;
                                        }
                                    },
                                    .construct => {
                                        const res = switch (op.lexeme[0]) {
                                            '=' => object1 == object2,
                                            '!' => object1 != object2,
                                            else => undefined,
                                        };
                                        return .{ .bool = res };
                                    },
                                    else => {},
                                }
                            },
                            else => {},
                        }
                    },
                }
            },
        }
        return error.UnexpectedType;
    }

    // Evaluate a closure given an argument
    fn evalClosure(self: *Interpreter, function: Value, clos: object.Closure, arg: *AST) EvalError!Value {
        try self.pushValue(function);
        const argument = try self.eval(arg);
        var copiedEnv = try clos.bound.clone();
        defer copiedEnv.deinit();
        try copiedEnv.put(clos.argName.lexeme, argument);
        self.popValue();
        try self.pushEnv(&copiedEnv);
        defer self.popEnv();
        switch (clos.code) {
            .ast => |ast| {
                return self.eval(ast);
            },
            .constructor => |constructor| {
                var contents = std.ArrayList(Value).init(self.allocator);
                errdefer contents.deinit();
                for (0..constructor.numArgs) |i| {
                    const argName = try std.fmt.allocPrint(
                        self.allocator,
                        "_{s}{d}",
                        .{ constructor.name, i },
                    );
                    defer self.allocator.free(argName);
                    try contents.append(self.lookup(argName).?);
                }
                return .{
                    .object = try self.objects.makeConstruct(constructor.name, contents),
                };
            },
        }
    }

    // Evaluate a call with a single argument
    fn evalCall(self: *Interpreter, function: Value, arg: *AST) !Value {
        switch (function) {
            .object => |obj| {
                switch (obj.content) {
                    .closure => |closure| {
                        return self.evalClosure(function, closure, arg);
                    },
                    .recurse => |rec| {
                        if (rec) |recVal| {
                            return self.evalCall(recVal, arg);
                        } else {
                            return error.UnknownIdentifier;
                        }
                    },
                    .multiArgClosure => |multiClos| {
                        try self.pushValue(function);
                        const argument = try self.eval(arg);
                        var copiedEnv = try multiClos.bound.clone();
                        // Arguments are stored in reverse
                        copiedEnv.put(multiClos.argNames.items[multiClos.argNames.items.len - 1].lexeme, argument) catch |err| {
                            copiedEnv.deinit();
                            return err;
                        };
                        self.popValue();
                        if (multiClos.argNames.items.len == 1) {
                            defer copiedEnv.deinit();
                            try self.pushEnv(&copiedEnv);
                            defer self.popEnv();
                            switch (multiClos.code) {
                                .ast => |ast| {
                                    const result = try self.eval(ast);
                                    return result;
                                },
                                .constructor => |constructor| {
                                    var contents = std.ArrayList(Value).init(self.allocator);
                                    errdefer contents.deinit();
                                    for (0..constructor.numArgs) |i| {
                                        const argName = try std.fmt.allocPrint(
                                            self.allocator,
                                            "_{s}{d}",
                                            .{ constructor.name, i },
                                        );
                                        defer self.allocator.free(argName);
                                        try contents.append(self.lookup(argName).?);
                                    }
                                    return .{
                                        .object = try self.objects.makeConstruct(constructor.name, contents),
                                    };
                                },
                            }
                        } else {
                            if (multiClos.argNames.items.len == 2) {
                                const closObj = try self.objects.makeClosure(
                                    multiClos.argNames.items[0],
                                    copiedEnv,
                                    multiClos.code,
                                );
                                return .{
                                    .object = closObj,
                                };
                            }
                            var copiedArgs = try multiClos.argNames.clone();
                            _ = copiedArgs.pop();
                            return .{
                                .object = try self.objects.makeMultiArgClosure(copiedArgs, copiedEnv, multiClos.code),
                            };
                        }
                    },
                    else => {
                        return error.UnexpectedType;
                    },
                }
            },
            else => {
                return error.UnexpectedType;
            },
        }
    }

    // Evaluate call with multiple arguments
    fn evalCallMult(self: *Interpreter, function: Value, args: *std.ArrayList(*AST)) !Value {
        switch (function) {
            .object => |obj| {
                switch (obj.content) {
                    .closure => |closure| {
                        // Arguments are stored in reverse
                        const result = try self.evalClosure(function, closure, args.items[args.items.len - 1]);
                        if (args.items.len == 1) {
                            return result;
                        } else if (args.items.len == 2) {
                            return self.evalCall(result, args.items[0]);
                        } else {
                            var copiedArgs = try args.clone();
                            defer copiedArgs.deinit();
                            _ = copiedArgs.pop();
                            return self.evalCallMult(result, &copiedArgs);
                        }
                    },
                    .recurse => |rec| {
                        if (rec) |recVal| {
                            return self.evalCallMult(recVal, args);
                        } else {
                            return error.UnknownIdentifier;
                        }
                    },
                    .multiArgClosure => |multiClos| {
                        try self.pushValue(function);
                        var copiedEnv = try multiClos.bound.clone();
                        const numArgs = args.items.len;
                        var i: usize = 0;
                        // Calculate arguments one by one,
                        // in reverse since stored that way
                        while (i < multiClos.argNames.items.len) : (i += 1) {
                            if (i < args.items.len) {
                                errdefer copiedEnv.deinit();
                                const argument = args.items[args.items.len - i - 1];
                                const arg = try self.eval(argument);
                                try self.pushValue(arg);
                                try copiedEnv.put(multiClos.argNames.items[multiClos.argNames.items.len - i - 1].lexeme, arg);
                            }
                        }
                        i = 0;
                        while (i < multiClos.argNames.items.len) : (i += 1) {
                            if (i < args.items.len) {
                                self.popValue();
                            }
                        }
                        // Pop the function
                        self.popValue();
                        if (numArgs >= multiClos.argNames.items.len) {
                            defer copiedEnv.deinit();
                            try self.pushEnv(&copiedEnv);
                            defer self.popEnv();
                            var result: Value = undefined;
                            switch (multiClos.code) {
                                .ast => |ast| {
                                    result = try self.eval(ast);
                                },
                                .constructor => |constructor| {
                                    var contents = std.ArrayList(Value).init(self.allocator);
                                    errdefer contents.deinit();
                                    for (0..constructor.numArgs) |j| {
                                        const argName = try std.fmt.allocPrint(
                                            self.allocator,
                                            "_{s}{d}",
                                            .{ constructor.name, j },
                                        );
                                        defer self.allocator.free(argName);
                                        try contents.append(self.lookup(argName).?);
                                    }
                                    result = .{
                                        .object = try self.objects.makeConstruct(constructor.name, contents),
                                    };
                                },
                            }
                            if (numArgs > multiClos.argNames.items.len) {
                                var copiedArgs = try args.clone();
                                defer copiedArgs.deinit();
                                i = 0;
                                while (i < numArgs) : (i += 1) {
                                    _ = copiedArgs.pop();
                                }
                                return self.evalCallMult(result, &copiedArgs);
                            }
                            return result;
                        } else {
                            if (multiClos.argNames.items.len - numArgs == 1) {
                                return .{ .object = try self.objects.makeClosure(
                                    multiClos.argNames.items[0],
                                    copiedEnv,
                                    multiClos.code,
                                ) };
                            }
                            var copiedArgs = try multiClos.argNames.clone();
                            errdefer copiedArgs.deinit();
                            i = 0;
                            while (i < numArgs) : (i += 1) {
                                _ = copiedArgs.pop();
                            }
                            return .{ .object = try self.objects.makeMultiArgClosure(
                                copiedArgs,
                                copiedEnv,
                                multiClos.code,
                            ) };
                        }
                    },
                    .construct => {
                        return error.UnexpectedType;
                    },
                }
            },
            else => {
                return error.UnexpectedType;
            },
        }
    }

    // Evaluate an expression
    pub fn eval(self: *Interpreter, ast: *AST) EvalError!Value {
        switch (ast.*) {
            .boolConstant => |boolC| {
                return .{ .bool = boolC.value };
            },
            .intConstant => |intC| {
                return .{ .int = intC.value };
            },
            .floatConstant => |floatC| {
                return .{ .float = floatC.value };
            },
            .ifExpr => |ifExpr| {
                const predicate = try self.eval(ifExpr.predicate);
                if (predicate.bool) {
                    return self.eval(ifExpr.thenExpr);
                } else {
                    return self.eval(ifExpr.elseExpr);
                }
            },
            .identifier => |id| {
                const value = self.lookup(id.token.lexeme);
                if (value) |val| {
                    switch (val) {
                        .object => |obj| {
                            switch (obj.content) {
                                .recurse => |rec| {
                                    if (rec) |recVal| {
                                        return recVal;
                                    } else {
                                        return error.UnknownIdentifier;
                                    }
                                },
                                else => {},
                            }
                        },
                        else => {},
                    }
                }
                return value orelse error.UnknownIdentifier;
            },
            .let => |let| {
                const prev = self.currentEnv.contents.get(let.name.lexeme);
                if (prev) |prevV| {
                    try self.pushValue(prevV);
                }
                // To enable recursion, a special object is put,
                // which contains an optional value that is then later changed
                // to the actual result
                const recursionPointer = try self.objects.makeRecurse();
                try self.set(let.name.lexeme, .{ .object = recursionPointer });
                const valOfVar = try self.eval(let.be);
                recursionPointer.content.recurse = valOfVar;
                try self.set(let.name.lexeme, valOfVar);
                const result = try self.eval(let.in);
                if (prev) |prevValue| {
                    self.popValue();
                    try self.set(let.name.lexeme, prevValue);
                } else {
                    _ = self.remove(let.name.lexeme);
                }
                return result;
            },
            .call => |call| {
                const function = try self.eval(call.function);
                return self.evalCall(function, call.arg);
            },
            .callMult => |*callMult| {
                const function = try self.eval(callMult.function);
                return self.evalCallMult(function, &callMult.args);
            },
            .lambda => |lambda| {
                // the boundEnv contains all captured values
                var boundEnv = std.StringHashMap(Value).init(self.allocator);
                errdefer boundEnv.deinit();
                const enclosed = lambda.encloses.?.items;
                var i: usize = 0;
                while (i < enclosed.len) : (i += 1) {
                    try boundEnv.put(enclosed[i], self.lookup(enclosed[i]).?);
                }
                const closure = try self.objects.makeClosure(lambda.argname, boundEnv, .{ .ast = lambda.expr });
                return .{ .object = closure };
            },
            .lambdaMult => |lambdaMult| {
                // the boundEnv contains all captured values
                var boundEnv = std.StringHashMap(Value).init(self.allocator);
                errdefer boundEnv.deinit();
                const enclosed = lambdaMult.encloses.items;
                var i: usize = 0;
                while (i < enclosed.len) : (i += 1) {
                    try boundEnv.put(enclosed[i], self.lookup(enclosed[i]).?);
                }
                const closure = try self.objects.makeMultiArgClosure(
                    try lambdaMult.argnames.clone(),
                    boundEnv,
                    .{ .ast = lambdaMult.expr },
                );
                return .{ .object = closure };
            },
            .operator => |op| {
                const left = try self.eval(op.left);
                try self.pushValue(left);
                const right = try self.eval(op.right);
                self.popValue();
                switch (op.token.lexeme[0]) {
                    '+', '-', '*', '/' => {
                        return evalNumberOp(op.token, left, right);
                    },
                    '<', '>', '=', '!' => {
                        return evalComp(op.token, left, right);
                    },
                    else => {
                        return undefined;
                    },
                }
            },
            .prefixOp => |prefixOp| {
                switch (prefixOp.token.lexeme[0]) {
                    '!' => {
                        const exprResult = try self.eval(prefixOp.expr);
                        return .{ .bool = !exprResult.bool };
                    },
                    '-' => {
                        const exprResult = try self.eval(prefixOp.expr);
                        switch (exprResult) {
                            .int => |int| {
                                return .{ .int = -int };
                            },
                            .float => |f| {
                                return .{ .float = -f };
                            },
                            else => {
                                return error.UnexpectedType;
                            },
                        }
                    },
                    else => {
                        return error.UnexpectedType;
                    },
                }
            },
            .case => |case| {
                const value = try self.eval(case.value);
                const construct = value.object.content.construct;
                for (case.patterns.items, case.bodies.items) |pattern, body| {
                    if (std.mem.eql(u8, pattern.name.lexeme, construct.name)) {
                        for (construct.values.items, pattern.values.items) |val, name| {
                            try self.set(name.lexeme, val);
                        }
                        return self.eval(body);
                    }
                }
                return error.UnexpectedType;
            },
        }
    }

    pub fn runStatement(self: *Interpreter, statement: Statement) !void {
        switch (statement) {
            .let => |let| {
                const recursionPointer = try self.objects.makeRecurse();
                try self.set(let.name.lexeme, .{ .object = recursionPointer });
                const val = try self.eval(let.be);
                recursionPointer.content.recurse = val;
                try self.set(let.name.lexeme, val);
            },
            .type => |typeDecl| {
                for (typeDecl.constructors.items) |constructor| {
                    if (constructor.args.items.len == 0) {
                        const contents = std.ArrayList(Value).init(self.allocator);
                        const construct = try self.objects.makeConstruct(constructor.name.lexeme, contents);
                        try self.set(constructor.name.lexeme, .{ .object = construct });
                        return;
                    }
                    try self.argNameMap.put(constructor.name.lexeme, .init(self.allocator));
                    var argNames = self.argNameMap.getPtr(constructor.name.lexeme).?;

                    for (0..constructor.args.items.len) |i| {
                        const argName = try std.fmt.allocPrint(self.allocator, "_{s}{d}", .{
                            constructor.name.lexeme,
                            constructor.args.items.len - i - 1,
                        });
                        errdefer self.allocator.free(argName);
                        try argNames.append(.{ .start = 0, .end = 0, .lexeme = argName, .type = .Identifier });
                    }
                    var bound = std.StringHashMap(Value).init(self.allocator);
                    errdefer bound.deinit();

                    const multiArgClosure = try self.objects.makeMultiArgClosure(try argNames.clone(), bound, .{ .constructor = .{
                        .numArgs = constructor.args.items.len,
                        .name = constructor.name.lexeme,
                    } });
                    try self.set(constructor.name.lexeme, .{ .object = multiArgClosure });
                }
            },
        }
    }
};
