const std = @import("std");
const token = @import("./token.zig");
const Value = @import("./value.zig").Value;
const AST = @import("./ast.zig").AST;
const Env = @import("./interpreter.zig").Env;

pub const Code = union(enum) {
    ast: *AST,
    constructor: Constructor,
};

pub const Closure = struct {
    argName: token.Token,
    bound: std.StringHashMap(Value),
    code: Code,
    fileName: []const u8,
    name: []const u8,
};

pub const MultiArgClosure = struct {
    argNames: std.ArrayList(token.Token),
    bound: std.StringHashMap(Value),
    code: Code,
    fileName: []const u8,
    name: []const u8,
};

pub const Constructor = struct {
    numArgs: usize,
    name: []const u8,
};

pub const ObjectContent = union(enum) {
    closure: Closure,
    multiArgClosure: MultiArgClosure,
    recurse: ?Value,
};

const GCStressTest: bool = false;
const GCLog: bool = false;

pub const Object = struct {
    content: ObjectContent,
    marked: bool,
    next: ?*Object,

    pub fn deinit(self: *Object, allocator: std.mem.Allocator) void {
        switch (self.content) {
            .closure => |*clos| {
                clos.bound.deinit();
            },
            .multiArgClosure => |*multiClos| {
                multiClos.bound.deinit();
                multiClos.argNames.deinit();
            },
            else => {},
        }
        allocator.destroy(self);
    }

    pub fn getType(self: *Object) []const u8 {
        return switch (self.content) {
            .closure => "Closure",
            .multiArgClosure => "MultiArgumentClosure",
            .recurse => "Recurse",
            .construct => "Construct",
        };
    }
};

pub const Objects = struct {
    newestObject: ?*Object = null,
    constructArrays: std.AutoHashMap([*]Value, struct { slice: []Value, marked: bool }),
    allocator: std.mem.Allocator,
    preserveValues: *std.ArrayList(Value),
    currentEnv: *Env,
    objCount: usize = 0,
    objCountUntilGC: usize = 100,

    file: []const u8,

    pub fn init(
        allocator: std.mem.Allocator,
        preserveValues: *std.ArrayList(Value),
        currentEnv: *Env,
    ) Objects {
        return .{
            .allocator = allocator,
            .preserveValues = preserveValues,
            .currentEnv = currentEnv,
            .file = "builtin",
            .constructArrays = .init(allocator),
        };
    }

    pub fn deinit(self: *Objects) void {
        while (self.newestObject) |object| {
            const next = object.next;
            object.deinit(self.allocator);
            self.newestObject = next;
        }
        var constructIterator = self.constructArrays.iterator();
        while (constructIterator.next()) |constructEntry| {
            self.allocator.free(constructEntry.value_ptr.slice);
        }
        self.constructArrays.deinit();
    }

    fn markObject(self: *Objects, obj: *Object) void {
        if (!obj.marked) {
            obj.marked = true;
            switch (obj.content) {
                .closure => |*clos| {
                    self.markMap(&clos.bound);
                },
                .multiArgClosure => |*multiClos| {
                    self.markMap(&multiClos.bound);
                },
                .recurse => |rec| {
                    if (rec) |recValue| {
                        self.markValue(recValue);
                    }
                },
            }
        }
    }

    fn markMap(self: *Objects, map: *std.StringHashMap(Value)) void {
        var iter = map.valueIterator();
        while (iter.next()) |value| {
            switch (value.*) {
                .object => |obj| {
                    self.markObject(obj);
                },
                else => {},
            }
        }
    }

    fn markEnv(self: *Objects, env: *Env) void {
        var iter = env.contents.valueIterator();
        while (iter.next()) |value| {
            self.markValue(value.*);
        }
        if (env.next) |nextEnv| {
            self.markEnv(nextEnv);
        }
    }

    pub fn markValue(self: *Objects, val: Value) void {
        switch (val) {
            .object => |obj| {
                self.markObject(obj);
            },
            .construct => |construct| {
                if (construct.values) |values| {
                    self.constructArrays.getPtr(values.ptr).?.marked = true;
                    for (values) |constructVal| {
                        self.markValue(constructVal);
                    }
                }
            },
            else => {},
        }
    }

    pub fn mark(self: *Objects) void {
        var i: usize = 0;
        while (i < self.preserveValues.items.len) : (i += 1) {
            self.markValue(self.preserveValues.items[i]);
        }
        self.markEnv(self.currentEnv);
    }

    pub fn sweep(self: *Objects) void {
        var o = self.newestObject;
        var prev: ?*Object = null;
        while (o) |obj| {
            o = obj.next;
            if (!obj.marked) {
                if (GCLog) {
                    std.debug.print("Deleted object of type {s}\n", .{obj.getType()});
                }
                obj.deinit(self.allocator);
                if (prev) |prevObj| {
                    prevObj.next = o;
                } else {
                    self.newestObject = o;
                }
            } else {
                obj.marked = false;
                prev = obj;
            }
        }
        var constructIterator = self.constructArrays.iterator();
        while (constructIterator.next()) |constructValues| {
            if (constructValues.value_ptr.marked) {
                self.allocator.free(constructValues.value_ptr.slice);
            }
            constructValues.value_ptr.marked = false;
        }
    }

    fn makeObject(self: *Objects) !*Object {
        if (GCStressTest or self.objCount > self.objCountUntilGC) {
            self.mark();
            self.sweep();
            if (self.objCount < self.objCountUntilGC / 4) {
                self.objCountUntilGC /= 2;
            } else if (self.objCount > self.objCountUntilGC) {
                self.objCountUntilGC *= 2;
            }
        }
        const object = try self.allocator.create(Object);
        object.next = self.newestObject;
        self.newestObject = object;
        return object;
    }

    pub fn makeClosure(self: *Objects, argName: token.Token, bound: std.StringHashMap(Value), code: Code) !*Object {
        const objectContent: ObjectContent = .{ .closure = .{
            .argName = argName,
            .bound = bound,
            .code = code,
            .fileName = self.file,
            .name = "_lambda_",
        } };
        const object = try self.makeObject();
        object.content = objectContent;
        return object;
    }

    pub fn makeMultiArgClosure(self: *Objects, argNames: std.ArrayList(token.Token), bound: std.StringHashMap(Value), code: Code) !*Object {
        const object = try self.makeObject();
        object.content = .{ .multiArgClosure = .{
            .argNames = argNames,
            .bound = bound,
            .code = code,
            .fileName = self.file,
            .name = "_lambda_",
        } };
        return object;
    }

    pub fn makeRecurse(self: *Objects) !*Object {
        const object = try self.makeObject();
        object.content = .{ .recurse = null };
        return object;
    }

    pub fn makeConstructor(self: *Objects, name: []const u8, numArgs: usize) !*Object {
        const object = try self.makeObject();
        object.content = .{ .constructor = .{
            .numArgs = numArgs,
            .name = name,
        } };
        return object;
    }

    pub fn makeConstruct(self: *Objects, name: []const u8, values: ?[]Value) !Value {
        if (values) |vals| {
            try self.constructArrays.put(vals.ptr, .{ .slice = vals, .marked = false });
        }
        return .{ .construct = .{
            .name = name,
            .values = values,
        } };
    }
};
