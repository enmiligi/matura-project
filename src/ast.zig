const std = @import("std");
const token = @import("./token.zig");
const Type = @import("./type_inference.zig").Type;
const type_inference = @import("./type_inference.zig");
const Region = @import("./errors.zig").Region;

pub const TypeAnnotation = struct {
    type: *Type,
    region: Region,
};

pub const Pattern = struct {
    name: token.Token,
    values: std.ArrayList(token.Token),

    pub fn print(self: *Pattern, writer: std.io.AnyWriter) !void {
        try writer.print("{s}", .{self.name.lexeme});
        for (self.values.items) |value| {
            try writer.print(" {s}", .{value.lexeme});
        }
    }

    pub fn deinit(self: *Pattern) void {
        self.values.deinit();
    }
};

pub const AST = union(enum) {
    let: struct {
        start: usize,
        name: token.Token,
        be: *AST,
        type: ?TypeAnnotation,
        in: *AST,
    },
    lambda: struct {
        start: usize,
        argname: token.Token,
        argType: ?TypeAnnotation,
        expr: *AST,
        encloses: ?std.ArrayList([]const u8) = null,
    },
    lambdaMult: struct {
        start: usize,
        argnames: std.ArrayList(token.Token),
        expr: *AST,
        encloses: std.ArrayList([]const u8),
    },
    ifExpr: struct {
        start: usize,
        predicate: *AST,
        thenExpr: *AST,
        elseExpr: *AST,
    },
    call: struct {
        function: *AST,
        arg: *AST,
    },
    callMult: struct {
        function: *AST,
        args: std.ArrayList(*AST),
    },
    intConstant: struct {
        token: token.Token,
        value: i64,
    },
    floatConstant: struct {
        token: token.Token,
        value: f64,
    },
    boolConstant: struct {
        token: token.Token,
        value: bool,
    },
    identifier: struct {
        token: token.Token,
    },
    operator: struct {
        token: token.Token,
        left: *AST,
        right: *AST,
    },
    prefixOp: struct {
        token: token.Token,
        expr: *AST,
    },
    case: struct {
        start: usize,
        value: *AST,
        patterns: std.ArrayList(Pattern),
        bodies: std.ArrayList(*AST),
    },
    list: struct {
        start: usize,
        end: usize,
        values: std.ArrayList(*AST),
    },

    // Recursively destroy all contained ASTs
    pub fn deinit(self: *AST, allocator: std.mem.Allocator) void {
        switch (self.*) {
            .let => |let| {
                let.be.deinit(allocator);
                let.in.deinit(allocator);
                if (let.type) |t| {
                    t.type.deinit(allocator);
                }
            },
            .lambda => |lambda| {
                lambda.expr.deinit(allocator);
                if (lambda.encloses) |encloses| {
                    encloses.deinit();
                }
                if (lambda.argType) |argType| {
                    argType.type.deinit(allocator);
                }
            },
            .lambdaMult => |lambdaMult| {
                lambdaMult.expr.deinit(allocator);
                lambdaMult.argnames.deinit();
                lambdaMult.encloses.deinit();
            },
            .call => |call| {
                call.function.deinit(allocator);
                call.arg.deinit(allocator);
            },
            .callMult => |callMult| {
                callMult.function.deinit(allocator);
                for (callMult.args.items) |arg| {
                    arg.deinit(allocator);
                }
                callMult.args.deinit();
            },
            .operator => |op| {
                op.left.deinit(allocator);
                op.right.deinit(allocator);
            },
            .ifExpr => |ifExpr| {
                ifExpr.predicate.deinit(allocator);
                ifExpr.thenExpr.deinit(allocator);
                ifExpr.elseExpr.deinit(allocator);
            },
            .prefixOp => |prefixOp| {
                prefixOp.expr.deinit(allocator);
            },
            .case => |case| {
                case.value.deinit(allocator);
                for (case.patterns.items) |*pattern| {
                    pattern.deinit();
                }
                case.patterns.deinit();
                for (case.bodies.items) |body| {
                    body.deinit(allocator);
                }
                case.bodies.deinit();
            },
            .list => |list| {
                for (list.values.items) |value| {
                    value.deinit(allocator);
                }
                list.values.deinit();
            },
            else => {},
        }
        allocator.destroy(self);
    }

    pub fn print(self: AST, writer: std.io.AnyWriter) !void {
        switch (self) {
            .ifExpr => |ifExpr| {
                try writer.print("If(predicate: ", .{});
                try ifExpr.predicate.print(writer);
                try writer.print(", then: ", .{});
                try ifExpr.thenExpr.print(writer);
                try writer.print(", else: ", .{});
                try ifExpr.elseExpr.print(writer);
                try writer.print(")", .{});
            },
            .let => |let| {
                try writer.print("Let(name: {s}, be: ", .{let.name.lexeme});
                try let.be.print(writer);
                try writer.print(", in: ", .{});
                try let.in.print(writer);
                try writer.print(")", .{});
            },
            .lambda => |lambda| {
                try writer.print("Lambda(argname: {s}, expr: ", .{lambda.argname.lexeme});
                try lambda.expr.print(writer);
                try writer.print(")", .{});
            },
            .lambdaMult => |lambdaMult| {
                try writer.print("LambdaMult(args: [", .{});
                // Arguments are stored in reverse order
                var i: usize = lambdaMult.argnames.items.len;
                while (i > 0) : (i -= 1) {
                    try writer.print("{s}", .{lambdaMult.argnames.items[i - 1].lexeme});
                    if (i != 1) {
                        try writer.print(", ", .{});
                    }
                }
                try writer.print("], expr: ", .{});
                try lambdaMult.expr.print(writer);
                try writer.print(")", .{});
            },
            .callMult => |callMult| {
                try writer.print("CallMult(function: ", .{});
                try callMult.function.print(writer);
                try writer.print(", args: [", .{});
                // Arguments are stored in reverse order
                var i: usize = callMult.args.items.len;
                while (i > 0) : (i -= 1) {
                    try callMult.args.items[i - 1].print(writer);
                    if (i != 1) {
                        try writer.print(", ", .{});
                    }
                }
                try writer.print("])", .{});
            },
            .call => |call| {
                try writer.print("Call(function: ", .{});
                try call.function.print(writer);
                try writer.print(", arg: ", .{});
                try call.arg.print(writer);
                try writer.print(")", .{});
            },
            .intConstant => |int| {
                try writer.print("Int({s})", .{int.token.lexeme});
            },
            .floatConstant => |float| {
                try writer.print("Float({s})", .{float.token.lexeme});
            },
            .boolConstant => |boolConst| {
                try writer.print("Bool({s})", .{boolConst.token.lexeme});
            },
            .identifier => |id| {
                try writer.print("Identifier({s})", .{id.token.lexeme});
            },
            .operator => |op| {
                try writer.print("(", .{});
                try op.left.print(writer);
                try writer.print("{s}", .{op.token.lexeme});
                try op.right.print(writer);
                try writer.print(")", .{});
            },
            .prefixOp => |prefixOp| {
                try writer.print("({s}", .{prefixOp.token.lexeme});
                try prefixOp.expr.print(writer);
                try writer.print(")", .{});
            },
            .case => |case| {
                try writer.print("Case(value: ", .{});
                try case.value.print(writer);
                try writer.print(", of: [", .{});
                for (case.patterns.items, 0..) |*pattern, i| {
                    try pattern.print(writer);
                    if (i != case.patterns.items.len - 1) {
                        try writer.print(", ", .{});
                    }
                }
                try writer.print("])", .{});
            },
            .list => |list| {
                try writer.write("[");
                for (list.values.items, 0..) |value, i| {
                    try value.print(writer);
                    if (i != list.values.items.len) {
                        try writer.write(", ");
                    }
                }
                try writer.write("]");
            },
        }
    }
};

pub const Statement = union(enum) {
    pub const Constructor = struct {
        name: token.Token,
        args: std.ArrayList(*Type),
    };

    let: struct {
        name: token.Token,
        be: *AST,
        annotation: ?TypeAnnotation,
    },
    type: struct {
        name: token.Token,
        compositeType: *Type,
        constructors: std.ArrayList(Constructor),
    },

    // Recursively destroy all contained ASTs
    pub fn deinit(self: *Statement, allocator: std.mem.Allocator) void {
        switch (self.*) {
            .let => |let| {
                let.be.deinit(allocator);
                if (let.annotation) |tA| {
                    tA.type.deinit(allocator);
                }
            },
            .type => |t| {
                for (t.constructors.items) |constructor| {
                    for (constructor.args.items) |arg| {
                        arg.deinit(allocator);
                    }
                    constructor.args.deinit();
                }
                t.constructors.deinit();
                t.compositeType.deinit(allocator);
            },
        }
    }

    pub fn print(self: Statement, writer: std.io.AnyWriter, allocator: std.mem.Allocator) !void {
        switch (self) {
            .let => |let| {
                try writer.print("Let(name: {s}, be: ", .{let.name.lexeme});
                try let.be.print(writer);
                try writer.print(")", .{});
            },
            .type => |t| {
                try writer.print("Type(name: {s}, constructors: [", .{t.name.lexeme});
                var i: usize = 0;
                var currentTypeVar: usize = 0;
                var typeVarMap = std.AutoHashMap(usize, usize).init(allocator);
                defer typeVarMap.deinit();
                while (i < t.constructors.items.len) : (i += 1) {
                    const constructor = &t.constructors.items[i];
                    try writer.print("{s}(", .{constructor.name.lexeme});
                    var j: usize = 0;
                    while (j < constructor.args.items.len) : (j += 1) {
                        try type_inference.printType(
                            constructor.args.items[j],
                            writer,
                            &currentTypeVar,
                            &typeVarMap,
                            true,
                            allocator,
                        );
                        if (j != constructor.args.items.len - 1) {
                            try writer.print(", ", .{});
                        }
                    }
                    try writer.print(")", .{});
                    if (i != t.constructors.items.len - 1) {
                        try writer.print(", ", .{});
                    }
                }
                try writer.print("])", .{});
            },
        }
    }

    pub fn deinitStatements(statements: std.ArrayList(Statement), allocator: std.mem.Allocator) void {
        for (statements.items) |*statement| {
            statement.deinit(allocator);
        }
        statements.deinit();
    }
};
