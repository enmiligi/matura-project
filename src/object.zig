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
};

pub const MultiArgClosure = struct {
    argNames: std.ArrayList(token.Token),
    bound: std.StringHashMap(Value),
    code: Code,
};

pub const Constructor = struct {
    numArgs: usize,
    name: []const u8,
};

pub const Construct = struct {
    name: []const u8,
    values: std.ArrayList(Value),
};

pub const ObjectContent = union(enum) {
    closure: Closure,
    multiArgClosure: MultiArgClosure,
    recurse: ?Value,
    construct: Construct,
};

const GCStressTest: bool = true;
const GCLog: bool = true;

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
            .construct => |construct| {
                construct.values.deinit();
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
    allocator: std.mem.Allocator,
    preserveValues: *std.ArrayList(Value),
    currentEnv: *Env,
    objCount: usize = 0,
    objCountUntilGC: usize = 100,

    pub fn init(
        allocator: std.mem.Allocator,
        preserveValues: *std.ArrayList(Value),
        currentEnv: *Env,
    ) Objects {
        return .{
            .allocator = allocator,
            .preserveValues = preserveValues,
            .currentEnv = currentEnv,
        };
    }

    pub fn deinit(self: *Objects) void {
        while (self.newestObject) |object| {
            const next = object.next;
            object.deinit(self.allocator);
            self.newestObject = next;
        }
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
                        switch (recValue) {
                            .object => |recObj| {
                                self.markObject(recObj);
                            },
                            else => {},
                        }
                    }
                },
                .construct => |construct| {
                    for (construct.values.items) |value| {
                        switch (value) {
                            .object => |valObj| {
                                self.markObject(valObj);
                            },
                            else => {},
                        }
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
            switch (value.*) {
                .object => |obj| {
                    self.markObject(obj);
                },
                else => {},
            }
        }
        if (env.next) |nextEnv| {
            self.markEnv(nextEnv);
        }
    }

    pub fn mark(self: *Objects) void {
        var i: usize = 0;
        while (i < self.preserveValues.items.len) : (i += 1) {
            switch (self.preserveValues.items[i]) {
                .object => |obj| {
                    self.markObject(obj);
                },
                else => {},
            }
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

    pub fn makeConstruct(self: *Objects, name: []const u8, values: std.ArrayList(Value)) !*Object {
        const object = try self.makeObject();
        object.content = .{ .construct = .{
            .values = values,
            .name = name,
        } };
        return object;
    }
};
