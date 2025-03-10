const object = @import("object.zig");
const std = @import("std");

pub const Value = union(enum) {
    int: i64,
    float: f64,
    object: *object.Object,
};

pub fn printValue(value: Value, writer: std.io.AnyWriter) !void {
    switch (value) {
        .float => |f| {
            try writer.print("{d}", .{f});
        },
        .int => |i| {
            try writer.print("{d}", .{i});
        },
        .object => |obj| {
            switch (obj.content) {
                .closure => {
                    try writer.print("Closure", .{});
                },
                .recurse => |rec| {
                    if (rec) |recurseVal| {
                        try printValue(recurseVal, writer);
                    } else {
                        try writer.print("Value used before defined", .{});
                    }
                },
            }
        },
    }
}
