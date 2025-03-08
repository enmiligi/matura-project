const std = @import("std");
const token = @import("./token.zig");
const AST = @import("./ast.zig").AST;
const Value = @import("./value.zig").Value;
const object = @import("./object.zig");

const EvalError = error{
    UnknownIdentifier,

    // This should never happen because of the type inference/checking
    UnexpectedType,
};

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

    pub fn eval(self: *Interpreter, ast: *AST) !Value {
        switch (ast.*) {
            .intConstant => |intC| {
                return .{ .int = intC.value };
            },
            .floatConstant => |floatC| {
                return .{ .float = floatC.value };
            },
            .identifier => |id| {
                return self.lookup(id.token.lexeme) orelse error.UnknownIdentifier;
            },
            .let => |let| {
                const valOfVar = try self.eval(let.be);
                const prev = self.lookup(let.name.lexeme);
                if (prev) |prevV| {
                    try self.pushValue(prevV);
                }
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
                switch (function) {
                    .object => |obj| {
                        switch (obj.content) {
                            .closure => |closure| {
                                try self.pushValue(function);
                                const arg = try self.eval(call.arg);
                                var copiedEnv = try closure.bound.clone();
                                defer copiedEnv.deinit();
                                try copiedEnv.put(closure.argName.lexeme, arg);
                                self.popValue();
                                try self.pushEnv(copiedEnv);
                                defer self.popEnv();
                                return self.eval(closure.code);
                            },
                        }
                    },
                    else => {
                        return error.UnexpectedType;
                    },
                }
            },
            .lambda => |lambda| {
                const boundEnv = try self.currentEnv.contents.clone();
                const closure = try self.objects.makeClosure(lambda.argname, boundEnv, lambda.expr);
                return .{ .object = closure };
            },
        }
    }
};
