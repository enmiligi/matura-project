const std = @import("std");
const token = @import("./token.zig");
const AST = @import("./ast.zig").AST;
const Value = @import("./value.zig").Value;
const object = @import("./object.zig");

const EvalError = error{
    UnknownIdentifier,

    // This should never happen because of the type inference/checking
    UnexpectedType,
} || std.mem.Allocator.Error;

pub const Env = struct {
    contents: std.StringHashMap(Value),
    next: ?*Env,
};

pub const Interpreter = struct {
    objects: object.Objects,
    preserveValues: *std.ArrayList(Value),
    currentEnv: *Env,
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) !Interpreter {
        const preserveValues = try allocator.create(std.ArrayList(Value));
        errdefer allocator.destroy(preserveValues);
        preserveValues.* = .init(allocator);
        const env = try allocator.create(Env);
        env.* = .{
            .contents = .init(allocator),
            .next = null,
        };
        return .{
            .objects = object.Objects.init(allocator, preserveValues, env),
            .preserveValues = preserveValues,
            .currentEnv = env,
            .allocator = allocator,
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
    }

    fn pushValue(self: *Interpreter, val: Value) !void {
        try self.preserveValues.append(val);
    }

    fn popValue(self: *Interpreter) void {
        _ = self.preserveValues.pop();
    }

    fn pushEnv(self: *Interpreter, map: std.StringHashMap(Value)) !void {
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

    inline fn lookup(self: *Interpreter, name: []const u8) ?Value {
        return self.currentEnv.contents.get(name);
    }

    inline fn set(self: *Interpreter, name: []const u8, val: Value) !void {
        try self.currentEnv.contents.put(name, val);
    }

    inline fn remove(self: *Interpreter, name: []const u8) void {
        _ = self.currentEnv.contents.remove(name);
    }

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

    fn evalClosure(self: *Interpreter, function: *Value, clos: *object.Closure, arg: *AST) EvalError!Value {
        try self.pushValue(function);
        const argument = try self.eval(arg);
        var copiedEnv = try clos.bound.clone();
        defer copiedEnv.deinit();
        try copiedEnv.put(clos.argName.lexeme, argument);
        self.popValue();
        try self.pushEnv(copiedEnv);
        defer self.popEnv();
        return self.eval(clos.code);
    }

    fn evalCall(self: *Interpreter, function: Value, arg: *AST) !Value {
        switch (function) {
            .object => |obj| {
                switch (obj.content) {
                    .closure => |closure| {
                        try self.pushValue(function);
                        const argument = try self.eval(arg);
                        var copiedEnv = try closure.bound.clone();
                        defer copiedEnv.deinit();
                        try copiedEnv.put(closure.argName.lexeme, argument);
                        self.popValue();
                        try self.pushEnv(copiedEnv);
                        defer self.popEnv();
                        return self.eval(closure.code);
                    },
                    .recurse => |rec| {
                        if (rec) |recVal| {
                            return self.evalCall(recVal, arg);
                        } else {
                            return error.UnknownIdentifier;
                        }
                    },
                }
            },
            else => {
                return error.UnexpectedType;
            },
        }
    }

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
                return self.lookup(id.token.lexeme) orelse error.UnknownIdentifier;
            },
            .let => |let| {
                const prev = self.lookup(let.name.lexeme);
                if (prev) |prevV| {
                    try self.pushValue(prevV);
                }
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
            .lambda => |lambda| {
                const boundEnv = try self.currentEnv.contents.clone();
                const closure = try self.objects.makeClosure(lambda.argname, boundEnv, lambda.expr);
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
        }
    }
};
