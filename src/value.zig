const object = @import("object.zig");
const std = @import("std");
const interpreter = @import("./interpreter.zig");

pub const Value = union(enum) {
    int: i64,
    float: f64,
    bool: bool,
    char: u8,
    object: *object.Object,
    builtinFunction: *const fn (self: *interpreter.Interpreter, arg: Value) anyerror!Value,
};

fn debugPrintChar(char: u8, writer: std.io.AnyWriter) !void {
    const out = switch (char) {
        '\n' => "\\n",
        '\r' => "\\r",
        '\t' => "\\t",
        '\'' => "\\'",
        '"' => "\\\"",
        else => {
            try writer.print("{c}", .{char});
            return;
        },
    };
    try writer.print("{s}", .{out});
}

pub fn debugPrintValue(value: Value, writer: std.io.AnyWriter) anyerror!void {
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
        .char => |c| {
            try writer.print("'", .{});
            try debugPrintChar(c, writer);
            try writer.print("'", .{});
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
                        try debugPrintValue(recurseVal, writer);
                    } else {
                        try writer.print("Value used before defined", .{});
                    }
                },
                .construct => |construct| {
                    if (std.mem.eql(u8, construct.name, "Cons")) {
                        const isString = switch (construct.values.items[0]) {
                            .char => true,
                            else => false,
                        };
                        if (isString) {
                            try writer.print("\"", .{});
                            try debugPrintChar(construct.values.items[0].char, writer);
                        } else {
                            try writer.print("[", .{});
                            try debugPrintValue(construct.values.items[0], writer);
                        }
                        var restList = construct.values.items[1];
                        while (true) {
                            switch (restList) {
                                .object => |restObj| {
                                    switch (restObj.content) {
                                        .construct => |restConstruct| {
                                            if (std.mem.eql(u8, restConstruct.name, "Cons")) {
                                                if (!isString) try writer.print(", ", .{});
                                                if (isString) {
                                                    try debugPrintChar(restConstruct.values.items[0].char, writer);
                                                } else {
                                                    try debugPrintValue(restConstruct.values.items[0], writer);
                                                }
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
                        if (isString) {
                            try writer.print("\"", .{});
                        } else {
                            try writer.print("]", .{});
                        }
                    } else {
                        try writer.print("{s}", .{construct.name});
                        for (construct.values.items) |val| {
                            switch (val) {
                                .object => |obj2| {
                                    switch (obj2.content) {
                                        .construct => |construct2| {
                                            if (construct2.values.items.len > 0) {
                                                try writer.print(" (", .{});
                                                try debugPrintValue(val, writer);
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
                            try debugPrintValue(val, writer);
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

pub fn printValue(value: Value, writer: std.io.AnyWriter) anyerror!void {
    switch (value) {
        .char => |c| {
            try writer.print("{c}", .{c});
        },
        .object => |obj| {
            switch (obj.content) {
                .construct => |construct| {
                    if (std.mem.eql(u8, construct.name, "Cons")) {
                        const isString = switch (construct.values.items[0]) {
                            .char => true,
                            else => false,
                        };
                        if (!isString) {
                            try writer.print("[", .{});
                        }
                        if (isString) {
                            try printValue(construct.values.items[0], writer);
                        } else {
                            try debugPrintValue(construct.values.items[0], writer);
                        }
                        var restList = construct.values.items[1];
                        while (true) {
                            switch (restList) {
                                .object => |restObj| {
                                    switch (restObj.content) {
                                        .construct => |restConstruct| {
                                            if (std.mem.eql(u8, restConstruct.name, "Cons")) {
                                                if (!isString) try writer.print(", ", .{});
                                                if (isString) {
                                                    try printValue(restConstruct.values.items[0], writer);
                                                } else {
                                                    try debugPrintValue(restConstruct.values.items[0], writer);
                                                }
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
                        if (!isString) {
                            try writer.print("]", .{});
                        }
                    } else {
                        try debugPrintValue(value, writer);
                    }
                },
                else => {
                    try debugPrintValue(value, writer);
                },
            }
        },
        else => {
            try debugPrintValue(value, writer);
        },
    }
}
