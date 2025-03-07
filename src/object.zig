const std = @import("std");
const token = @import("./token.zig");
const Value = @import("./value.zig").Value;
const AST = @import("./ast.zig").AST;

pub const Closure = struct {
    argName: token.Token,
    bound: std.StringHashMap(Value),
    code: *AST,
};

pub const ObjectContent = union(enum) {
    closure: Closure,
};

pub const Object = struct {
    content: ObjectContent,
    next: ?*Object,
};

pub const Objects = struct {
    newestObject: ?*Object = null,
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) Objects {
        return .{ .allocator = allocator };
    }

    pub fn deinit(self: *Objects) void {
        while (self.newestObject) |object| {
            switch (object.content) {
                .closure => |*clos| {
                    clos.bound.deinit();
                    self.newestObject = object.next;
                    self.allocator.destroy(object);
                },
            }
        }
    }

    pub fn makeClosure(self: *Objects, argName: token.Token, bound: std.StringHashMap(Value), code: *AST) !*Object {
        const object = try self.allocator.create(Object);
        object.next = self.newestObject;
        object.content = .{ .closure = .{
            .argName = argName,
            .bound = bound,
            .code = code,
        } };
        self.newestObject = object;
        return object;
    }
};
