const object = @import("object.zig");
const std = @import("std");
const interpreter = @import("./interpreter.zig");

pub const Value = union(enum) {
    int: i64,
    float: f64,
    bool: bool,
    object: *object.Object,
    builtinFunction: *const fn (self: *interpreter.Interpreter, arg: Value) anyerror!Value,
};

pub fn printValue(value: Value, writer: std.io.AnyWriter) !void {
    switch (value) {
        .float => |f| {
            try writer.print("{d}", .{f});
        },
        .int => |i| {
            try writer.print("{d}", .{i});
        },
        .bool => |b| {
            if (b) {
                try writer.print("True", .{});
            } else {
                try writer.print("False", .{});
            }
        },
        .object => |obj| {
            switch (obj.content) {
                .closure => {
                    try writer.print("Closure", .{});
                },
                .multiArgClosure => {
                    try writer.print("Closure", .{});
                },
                .recurse => |rec| {
                    if (rec) |recurseVal| {
                        try printValue(recurseVal, writer);
                    } else {
                        try writer.print("Value used before defined", .{});
                    }
                },
                .construct => |construct| {
                    if (std.mem.eql(u8, construct.name, "Cons")) {
                        try writer.print("[", .{});
                        try printValue(construct.values.items[0], writer);
                        var restList = construct.values.items[1];
                        while (true) {
                            switch (restList) {
                                .object => |restObj| {
                                    switch (restObj.content) {
                                        .construct => |restConstruct| {
                                            if (std.mem.eql(u8, restConstruct.name, "Cons")) {
                                                try writer.print(", ", .{});
                                                try printValue(restConstruct.values.items[0], writer);
                                                restList = restConstruct.values.items[1];
                                            } else {
                                                break;
                                            }
                                        },
                                        else => {
                                            unreachable;
                                        },
                                    }
                                },
                                else => {
                                    unreachable;
                                },
                            }
                        }
                        try writer.print("]", .{});
                    } else {
                        try writer.print("{s}", .{construct.name});
                        for (construct.values.items) |val| {
                            switch (val) {
                                .object => |obj2| {
                                    switch (obj2.content) {
                                        .construct => |construct2| {
                                            if (construct2.values.items.len > 0) {
                                                try writer.print(" (", .{});
                                                try printValue(val, writer);
                                                try writer.print(")", .{});
                                                continue;
                                            }
                                        },
                                        else => {},
                                    }
                                },
                                else => {},
                            }
                            try writer.print(" ", .{});
                            try printValue(val, writer);
                        }
                    }
                },
            }
        },
        .builtinFunction => {
            try writer.print("Builtin function", .{});
        },
    }
}
