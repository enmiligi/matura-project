const std = @import("std");
const token = @import("./token.zig");
const AST = @import("./ast.zig").AST;
const Statement = @import("./ast.zig").Statement;
const value = @import("./value.zig");
const Value = value.Value;
const object = @import("./object.zig");
const errors = @import("./errors.zig");

const nullToken = token.Token{ .start = 0, .end = 0, .lexeme = "", .type = .Identifier };

pub const EvalError = error{
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
    stdout: std.io.AnyWriter,
    stdoutbw: *std.io.BufferedWriter(4096, std.io.AnyWriter),
    stdin: std.io.AnyReader,

    argNameMap: std.StringHashMap(std.ArrayList(token.Token)),

    pub fn init(
        allocator: std.mem.Allocator,
        initialEnv: *std.StringHashMap(Value),
        stdout: std.io.AnyWriter,
        stdoutbw: *std.io.BufferedWriter(4096, std.io.AnyWriter),
        stdin: std.io.AnyReader,
    ) !Interpreter {
        try initBuiltins(initialEnv);
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
            .stdout = stdout,
            .stdoutbw = stdoutbw,
            .stdin = stdin,
        };
    }

    fn initBuiltins(env: *std.StringHashMap(Value)) !void {
        try env.put("print", .{ .builtinFunction = &print });
        try env.put("read", .{ .builtinFunction = &read });
        try env.put("parseInt", .{ .builtinFunction = &parseInt });
        try env.put("parseFloat", .{ .builtinFunction = &parseFloat });
        try env.put("showInt", .{ .builtinFunction = &showInt });
        try env.put("showFloat", .{ .builtinFunction = &showFloat });
    }

    fn print(self: *Interpreter, arg: Value) !Value {
        try value.printValue(arg, self.stdout);
        const values = std.ArrayList(Value).init(self.allocator);
        const composite = try self.objects.makeConstruct("Void", values);
        return .{ .object = composite };
    }

    fn read(self: *Interpreter, arg: Value) !Value {
        _ = arg;
        try self.stdoutbw.flush();
        const line = try self.stdin.readUntilDelimiterOrEofAlloc(
            self.allocator,
            '\n',
            std.math.maxInt(usize),
        ) orelse "";
        defer self.allocator.free(line);
        var listVal = try self.objects.makeConstruct("Nil", .init(self.allocator));
        for (1..line.len + 1) |i| {
            var vals = std.ArrayList(Value).init(self.allocator);
            errdefer vals.deinit();
            try vals.append(.{ .char = line[line.len - i] });
            try vals.append(.{ .object = listVal });
            try self.pushValue(.{ .object = listVal });
            listVal = try self.objects.makeConstruct("Cons", vals);
            self.popValue();
        }
        return .{ .object = listVal };
    }

    fn listToString(self: *Interpreter, list: Value) !std.ArrayList(u8) {
        var values = std.ArrayList(u8).init(self.allocator);
        errdefer values.deinit();
        var rest = list;
        while (std.mem.eql(u8, rest.object.content.construct.name, "Cons")) {
            try values.append(rest.object.content.construct.values.items[0].char);
            rest = rest.object.content.construct.values.items[1];
        }
        return values;
    }

    fn stringToList(self: *Interpreter, chars: []const u8) !Value {
        var list = try self.objects.makeConstruct("Nil", .init(self.allocator));
        for (1..chars.len + 1) |i| {
            var values = std.ArrayList(Value).init(self.allocator);
            try values.append(.{ .char = chars[chars.len - i] });
            try values.append(.{ .object = list });
            try self.pushValue(.{ .object = list });
            list = try self.objects.makeConstruct("Cons", values);
            self.popValue();
        }
        return .{ .object = list };
    }

    fn parseInt(self: *Interpreter, arg: Value) !Value {
        const string = try self.listToString(arg);
        defer string.deinit();
        const number: i64 = std.fmt.parseInt(i64, string.items, 10) catch {
            return .{
                .object = try self.objects.makeConstruct("None", .init(self.allocator)),
            };
        };
        var contents: std.ArrayList(Value) = .init(self.allocator);
        try contents.append(.{ .int = number });
        return .{ .object = try self.objects.makeConstruct("Some", contents) };
    }

    fn parseFloat(self: *Interpreter, arg: Value) !Value {
        const string = try self.listToString(arg);
        defer string.deinit();
        const number: f64 = std.fmt.parseFloat(f64, string.items) catch {
            return .{
                .object = try self.objects.makeConstruct("None", .init(self.allocator)),
            };
        };
        var contents: std.ArrayList(Value) = .init(self.allocator);
        try contents.append(.{ .float = number });
        return .{ .object = try self.objects.makeConstruct("Some", contents) };
    }

    fn showInt(self: *Interpreter, arg: Value) !Value {
        const string = try std.fmt.allocPrint(self.allocator, "{d}", .{arg.int});
        defer self.allocator.free(string);
        return self.stringToList(string);
    }

    fn showFloat(self: *Interpreter, arg: Value) !Value {
        var string: []const u8 = undefined;
        if (std.math.floor(arg.float) == arg.float) {
            string = try std.fmt.allocPrint(self.allocator, "{d}.0", .{arg.float});
        } else {
            string = try std.fmt.allocPrint(self.allocator, "{d}", .{arg.float});
        }
        defer self.allocator.free(string);
        return self.stringToList(string);
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
            .char => |char1| {
                switch (right) {
                    .char => |char2| {
                        const res = switch (op.lexeme[0]) {
                            '=' => char1 == char2,
                            '!' => char1 != char2,
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
                            .builtinFunction => {
                                return switch (op.lexeme[0]) {
                                    '=' => .{ .bool = false },
                                    '!' => .{ .bool = true },
                                    else => undefined,
                                };
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
                            .builtinFunction => {
                                return switch (op.lexeme[0]) {
                                    '=' => .{ .bool = false },
                                    '!' => .{ .bool = true },
                                    else => undefined,
                                };
                            },
                            else => {},
                        }
                    },
                    .construct => |construct1| {
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
                                    .construct => |construct2| {
                                        switch (op.lexeme[0]) {
                                            '=' => {
                                                if (!std.mem.eql(u8, construct1.name, construct2.name)) {
                                                    return .{ .bool = false };
                                                }
                                                var equal = true;
                                                for (construct1.values.items, construct2.values.items) |value1, value2| {
                                                    equal = equal and (try evalComp(op, value1, value2)).bool;
                                                }
                                                return .{ .bool = equal };
                                            },
                                            '!' => {
                                                if (!std.mem.eql(u8, construct1.name, construct2.name)) {
                                                    return .{ .bool = true };
                                                }
                                                var different = false;
                                                for (construct1.values.items, construct2.values.items) |value1, value2| {
                                                    different = different or (try evalComp(op, value1, value2)).bool;
                                                }
                                                return .{ .bool = different };
                                            },
                                            else => {},
                                        }
                                    },
                                    else => {},
                                }
                            },
                            else => {},
                        }
                    },
                }
            },
            .builtinFunction => |builtin1| {
                switch (right) {
                    .builtinFunction => |builtin2| {
                        const res = switch (op.lexeme[0]) {
                            '=' => builtin1 == builtin2,
                            '!' => builtin1 != builtin2,
                            else => undefined,
                        };
                        return .{ .bool = res };
                    },
                    else => {
                        const res = switch (op.lexeme[0]) {
                            '=' => false,
                            '!' => true,
                            else => undefined,
                        };
                        return .{ .bool = res };
                    },
                }
            },
        }
        return error.UnexpectedType;
    }

    // Evaluate a closure given an argument
    fn evalClosure(self: *Interpreter, function: Value, clos: object.Closure, arg: *AST) !Value {
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
                        defer self.popValue();
                        const argument = try self.eval(arg);
                        try self.pushValue(argument);
                        defer self.popValue();
                        var copiedEnv = try multiClos.bound.clone();
                        // Arguments are stored in reverse
                        copiedEnv.put(multiClos.argNames.items[multiClos.argNames.items.len - 1].lexeme, argument) catch |err| {
                            copiedEnv.deinit();
                            return err;
                        };
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
                                const argName = multiClos.argNames.items[0];
                                const code = multiClos.code;
                                const closObj = try self.objects.makeClosure(
                                    argName,
                                    copiedEnv,
                                    code,
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
            .builtinFunction => |builtin| {
                const argument = try self.eval(arg);
                return builtin(self, argument);
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
            .builtinFunction => |builtin| {
                // Arguments are stored in reverse
                const result = try builtin(self, try self.eval(args.items[args.items.len - 1]));
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
            else => {
                return error.UnexpectedType;
            },
        }
    }

    // Evaluate an expression
    pub fn eval(self: *Interpreter, ast: *AST) anyerror!Value {
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
            .charConstant => |charC| {
                return .{ .char = charC.value };
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
                const idValue = self.lookup(id.token.lexeme);
                if (idValue) |val| {
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
                return idValue orelse error.UnknownIdentifier;
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
                    ';' => {
                        return right;
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
                const caseValue = try self.eval(case.value);
                try self.pushValue(caseValue);
                defer self.popValue();
                const construct = caseValue.object.content.construct;
                for (case.patterns.items, case.bodies.items) |pattern, body| {
                    if (std.mem.eql(u8, pattern.name.lexeme, construct.name)) {
                        for (construct.values.items, pattern.values.items) |val, name| {
                            try self.set(name.lexeme, val);
                            try self.pushValue(val);
                        }
                        const result = self.eval(body);
                        for (0..construct.values.items.len) |_| {
                            self.popValue();
                        }
                        return result;
                    }
                }
                return error.UnexpectedType;
            },
            .list => {
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

    pub fn runMain(self: *Interpreter) !void {
        const voidAst = try self.allocator.create(AST);
        defer voidAst.deinit(self.allocator);
        voidAst.* = .{ .identifier = .{ .token = .{
            .start = 0,
            .end = 0,
            .lexeme = "Void",
            .type = .Identifier,
        } } };
        _ = try self.evalCall(self.lookup("main").?, voidAst);
    }
};
