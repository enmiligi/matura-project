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

pub const Interpreter = struct {
    objects: object.Objects,

    pub fn init(allocator: std.mem.Allocator) Interpreter {
        return .{
            .objects = object.Objects.init(allocator),
        };
    }

    pub fn deinit(self: *Interpreter) void {
        self.objects.deinit();
    }

    pub fn eval(self: *Interpreter, ast: *AST, env: *std.StringHashMap(Value)) !Value {
        switch (ast.*) {
            .intConstant => |intC| {
                return .{ .int = intC.value };
            },
            .floatConstant => |floatC| {
                return .{ .float = floatC.value };
            },
            .identifier => |id| {
                return env.get(id.token.lexeme) orelse error.UnknownIdentifier;
            },
            .let => |let| {
                const valOfVar = try self.eval(let.be, env);
                const prev = env.get(let.name.lexeme);
                try env.put(let.name.lexeme, valOfVar);
                const result = try self.eval(let.in, env);
                if (prev) |prevValue| {
                    try env.put(let.name.lexeme, prevValue);
                } else {
                    _ = env.remove(let.name.lexeme);
                }
                return result;
            },
            .call => |call| {
                const function = try self.eval(call.function, env);
                switch (function) {
                    .object => |obj| {
                        switch (obj.content) {
                            .closure => |closure| {
                                const arg = try self.eval(call.arg, env);
                                var copiedEnv = try closure.bound.clone();
                                defer copiedEnv.deinit();
                                try copiedEnv.put(closure.argName.lexeme, arg);
                                return self.eval(closure.code, &copiedEnv);
                            },
                        }
                    },
                    else => {
                        return error.UnexpectedType;
                    },
                }
            },
            .lambda => |lambda| {
                const boundEnv = try env.clone();
                const closure = try self.objects.makeClosure(lambda.argname, boundEnv, lambda.expr);
                return .{ .object = closure };
            },
        }
    }
};
