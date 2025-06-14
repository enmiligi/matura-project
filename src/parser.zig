const std = @import("std");
const token = @import("./token.zig");
const lexer = @import("./lexer.zig");
const _ast = @import("./ast.zig");
const AST = _ast.AST;
const TypeAnnotation = _ast.TypeAnnotation;
const Statement = _ast.Statement;
const Pattern = _ast.Pattern;
const errors = @import("./errors.zig");
const type_inference = @import("./type_inference.zig");
const utils = @import("./utils.zig");

const Precedence = usize;
const PrefixParseFn = *const fn (self: *Parser) anyerror!*AST;
const InfixParseFn = *const fn (self: *Parser, left: *AST) anyerror!*AST;
const InfixParseRule = struct { InfixParseFn, Precedence };

const builtinTypes = std.StaticStringMap(void).initComptime(.{
    .{ "Int", undefined },
    .{ "Float", undefined },
    .{ "Bool", undefined },
});

pub const ParserError = error{
    InvalidPrefix,
    UnexpectedToken,
    InvalidType,
};

pub const Parser = struct {
    allocator: std.mem.Allocator,
    lexer: *lexer.Lexer,
    next: token.Token,
    atEnd: bool = false,
    errs: *errors.Errors,
    algorithmJ: *type_inference.AlgorithmJ,
    typeVarMap: ?std.StringHashMap(*type_inference.Type),

    // Initialize parser and Lexer
    pub fn init(
        allocator: std.mem.Allocator,
        errs: *errors.Errors,
        algorithmJ: *type_inference.AlgorithmJ,
    ) !Parser {
        const l = try allocator.create(lexer.Lexer);
        l.* = .{ .source = undefined, .errs = errs };
        return .{
            .allocator = allocator,
            .lexer = l,
            .next = undefined,
            .errs = errs,
            .algorithmJ = algorithmJ,
            .typeVarMap = null,
        };
    }

    // Put a type parsed from tExpr in the env at the name given
    fn initBuiltin(self: *Parser, name: []const u8, tExpr: []const u8) !void {
        try self.newSource(tExpr);
        var region: utils.Region = .{ .start = 0, .end = 0 };
        const t = try self.constrainedType(&region, false, 0);
        const tS = self.algorithmJ.generalise(t, 0) catch |err| {
            t.deinit(self.allocator);
            return err;
        };
        errdefer type_inference.deinitScheme(tS, self.allocator);
        try self.algorithmJ.globalTypes.put(name, tS);
    }

    // Create and initialize the types of the builtin functions
    pub fn initBuiltins(self: *Parser) !void {
        self.typeVarMap = .init(self.allocator);
        defer {
            var iterator = self.typeVarMap.?.valueIterator();
            while (iterator.next()) |value| {
                value.*.deinit(self.allocator);
            }
            self.typeVarMap.?.deinit();
            self.typeVarMap = null;
        }
        try self.initBuiltin("print", "a -> Void");
        try self.initBuiltin("read", "Void -> List Char");
        try self.initBuiltin("parseInt", "List Char -> Option Int");
        try self.initBuiltin("parseFloat", "List Char -> Option Float");
        try self.initBuiltin("showInt", "Int -> List Char");
        try self.initBuiltin("showFloat", "Float -> List Char");
        try self.initBuiltin("trace", "Void -> Void");
    }

    // Switch the source code of the parser to another file
    pub fn newSource(self: *Parser, source: []const u8) !void {
        self.lexer.newSource(source);
        self.next = try self.lexer.getToken();
    }

    pub fn deinit(self: *Parser) void {
        self.allocator.destroy(self.lexer);
    }

    // Parse a constraint and put it into the typeVarMap
    // constraint ::= Number Identifier
    fn constraint(self: *Parser) !?usize {
        if (self.peekToken().type == .Identifier and std.mem.eql(u8, self.peekToken().lexeme, "Number")) {
            const numberToken = try self.getToken();
            if (self.typeVar()) |typeVarName| {
                const t = try type_inference.Type.init(self.allocator);
                if (self.typeVarMap.?.get(typeVarName.lexeme)) |tV| {
                    tV.rc += 1;
                    switch (tV.data) {
                        .typeVar => {
                            t.data = .{ .number = .{ .variable = tV } };
                        },
                        else => {
                            errdefer self.allocator.destroy(t);
                            const newTypeVar = try type_inference.Type.init(self.allocator);
                            newTypeVar.* = self.algorithmJ.newVarT();
                            newTypeVar.data.typeVar.declaration = true;
                            newTypeVar.data.typeVar.subst = tV;
                            t.data = .{ .number = .{ .variable = newTypeVar } };
                        },
                    }
                } else {
                    errdefer self.allocator.destroy(t);
                    const newTypeVar = try type_inference.Type.init(self.allocator);
                    newTypeVar.* = self.algorithmJ.newVarT();
                    newTypeVar.data.typeVar.declaration = true;
                    t.data = .{ .number = .{ .variable = newTypeVar } };
                }
                errdefer t.deinit(self.allocator);
                try self.typeVarMap.?.put(typeVarName.lexeme, t);
                return numberToken.start;
            } else {
                try self.errs.errorAt(self.peekToken().start, self.peekToken().end, "Expected this to be a type variable.", .{});
            }
        }
        return null;
    }

    // Parse a type with optionally constraints
    pub fn constrainedType(self: *Parser, region: *utils.Region, declaredVars: bool, precedence: u8) !*type_inference.Type {
        var start: usize = 0;
        while (try self.constraint()) |constraintStart| {
            if (start == 0) {
                start = constraintStart;
            }
            if (self.peekToken().type == .Comma) {
                _ = try self.getToken();
            }
        }
        if (start != 0) {
            _ = try self.expectToken(.DoubleArrow);
        }
        const result = try self.typeExpr(region, declaredVars, precedence);
        if (start != 0) {
            region.start = start;
        }
        return result;
    }

    // Parse a type without constraints
    // Arguments given to type have highest precedence, then function types
    pub fn typeExpr(self: *Parser, region: *utils.Region, declaredVars: bool, precedence: u8) !*type_inference.Type {
        var currentType: *type_inference.Type = undefined;
        if (self.typeVar()) |typeVarName| {
            if (region.start == 0) {
                region.start = typeVarName.start;
            }
            region.end = typeVarName.end;
            if (self.typeVarMap.?.get(typeVarName.lexeme)) |tV| {
                currentType = tV;
                tV.rc += 1;
            } else {
                if (declaredVars) {
                    try self.errs.errorAt(
                        typeVarName.start,
                        typeVarName.end,
                        "The typevar '{s}' was not declared.",
                        .{typeVarName.lexeme},
                    );
                    return error.UnexpectedToken;
                }
                var newTypeVar = try type_inference.Type.init(self.allocator);
                errdefer newTypeVar.deinit(self.allocator);
                newTypeVar.* = self.algorithmJ.newVarT();
                newTypeVar.data.typeVar.declaration = true;
                try self.typeVarMap.?.put(typeVarName.lexeme, newTypeVar);
                newTypeVar.rc += 1;
                currentType = newTypeVar;
            }
        } else if (self.typeName()) |tN| {
            if (region.start == 0) {
                region.start = tN.start;
            }
            region.end = tN.end;
            currentType = try type_inference.Type.init(self.allocator);
            errdefer self.allocator.destroy(currentType);
            if (std.mem.eql(u8, tN.lexeme, "Int")) {
                currentType.data = .{ .primitive = .Int };
            } else if (std.mem.eql(u8, tN.lexeme, "Float")) {
                currentType.data = .{ .primitive = .Float };
            } else if (std.mem.eql(u8, tN.lexeme, "Bool")) {
                currentType.data = .{ .primitive = .Bool };
            } else if (std.mem.eql(u8, tN.lexeme, "Char")) {
                currentType.data = .{ .primitive = .Char };
            } else if (self.algorithmJ.composite.get(tN.lexeme)) |compositeType| {
                if (compositeType.numVars > 0 and precedence >= 1) {
                    try self.errs.errorAt(
                        tN.start,
                        tN.end,
                        "This type needs a type as an argument, did you forget parentheses?",
                        .{},
                    );
                    return error.UnexpectedToken;
                }
                var args: std.ArrayList(*type_inference.Type) = .init(self.allocator);
                errdefer {
                    for (args.items) |arg| {
                        arg.deinit(self.allocator);
                    }
                    args.deinit();
                }
                var i: usize = 0;
                while (i < compositeType.numVars) : (i += 1) {
                    var _region: utils.Region = .{ .start = 0, .end = 0 };
                    const arg = try self.typeExpr(&_region, declaredVars, 1);
                    try args.append(arg);
                    region.end = _region.end;
                }
                currentType.data = .{ .composite = .{ .name = tN.lexeme, .args = args } };
            } else {
                try self.errs.errorAt(
                    tN.start,
                    tN.end,
                    "The type {s} does not exist.",
                    .{tN.lexeme},
                );
                return error.UnexpectedToken;
            }
        } else if (self.peekToken().type == .LeftParen) {
            const t = try self.getToken();
            if (region.start == 0) {
                region.start = t.start;
            }
            var r: utils.Region = .{ .start = 0, .end = 0 };
            currentType = try self.typeExpr(&r, declaredVars, 0);
            errdefer currentType.deinit(self.allocator);
            region.end = (try self.expectToken(.RightParen)).end;
        } else {
            try self.errs.errorAt(
                self.peekToken().start,
                self.peekToken().end,
                "Didn't expect {s} at start of type.",
                .{token.formatTokenType(self.peekToken().type)},
            );
            return error.UnexpectedToken;
        }
        if (precedence == 0 and self.peekToken().type == .Arrow) {
            _ = try self.getToken();
            errdefer currentType.deinit(self.allocator);
            var innerRegion: utils.Region = .{ .start = 0, .end = 0 };
            const returnType = try self.typeExpr(&innerRegion, declaredVars, 0);
            region.end = innerRegion.end;
            errdefer returnType.deinit(self.allocator);
            const argumentType = currentType;
            currentType = try type_inference.Type.init(self.allocator);
            currentType.data = .{ .function = .{
                .from = argumentType,
                .to = returnType,
            } };
        }
        if (precedence == 0 and (self.peekToken().type == .LeftParen or self.peekToken().type == .Identifier)) {
            currentType.deinit(self.allocator);
            try self.errs.errorAt(
                self.peekToken().start,
                self.peekToken().end,
                "This type doesn't need an additional argument.",
                .{},
            );
            return error.UnexpectedToken;
        }
        return currentType;
    }

    // file ::= statement*
    pub fn file(self: *Parser) !std.ArrayList(Statement) {
        var statements = std.ArrayList(Statement).init(self.allocator);
        errdefer Statement.deinitStatements(statements, self.allocator);
        while (self.peekToken().type != .EOF) {
            const stmt = try self.statement();
            try statements.append(stmt);
            switch (self.peekToken().type) {
                .NewStatement => {
                    _ = try self.getToken();
                },
                .EOF => {},
                else => {
                    try self.errs.errorAt(
                        self.peekToken().start,
                        self.peekToken().end,
                        "Didn't expect {s}.",
                        .{token.formatTokenType(self.peekToken().type)},
                    );
                    return error.UnexpectedToken;
                },
            }
        }
        return statements;
    }

    // Parse a let or type statement
    fn statement(self: *Parser) !Statement {
        switch (self.peekToken().type) {
            .Let => {
                return self.letStatement();
            },
            .Type => {
                return self.typeStatement();
            },
            .NewStatement => {
                _ = try self.getToken();
                return self.statement();
            },
            else => {
                try self.errs.errorAt(
                    self.peekToken().start,
                    self.peekToken().end,
                    "Didn't expect {s} at start of statement.",
                    .{token.formatTokenType(self.peekToken().type)},
                );
                return error.UnexpectedToken;
            },
        }
    }

    // A type variable has a lowercase name
    fn typeVar(self: *Parser) ?token.Token {
        if (self.peekToken().type == .Identifier) {
            if (std.ascii.isLower(self.peekToken().lexeme[0])) {
                return self.getToken() catch {
                    return null;
                };
            }
        }
        return null;
    }

    // A type name starts with an uppercase letter
    fn typeName(self: *Parser) ?token.Token {
        if (self.peekToken().type == .Identifier) {
            if (std.ascii.isUpper(self.peekToken().lexeme[0])) {
                return self.getToken() catch {
                    return null;
                };
            }
        }
        return null;
    }

    // letStatement ::= Let Identifier Equal expr
    fn letStatement(self: *Parser) !Statement {
        _ = try self.getToken();

        const name = try self.expectToken(.Identifier);

        var typeAnnotation: ?TypeAnnotation = null;
        errdefer {
            if (typeAnnotation) |tA| {
                tA.type.deinit(self.allocator);
            }
        }
        if (self.peekToken().type == .Colon) {
            _ = try self.getToken();
            var typeRegion: utils.Region = .{ .start = 0, .end = 0 };
            self.typeVarMap = .init(self.allocator);
            defer {
                var iterator = self.typeVarMap.?.valueIterator();
                while (iterator.next()) |value| {
                    value.*.deinit(self.allocator);
                }
                self.typeVarMap.?.deinit();
                self.typeVarMap = null;
            }
            const t = try self.constrainedType(&typeRegion, false, 0);
            typeAnnotation = .{ .type = t, .region = typeRegion };
        }

        _ = try self.expectToken(.Equal);

        const be = try self.expression(0);
        errdefer be.deinit(self.allocator);

        const letStmt: Statement = .{ .let = .{
            .name = name,
            .be = be,
            .annotation = typeAnnotation,
        } };
        return letStmt;
    }

    // constructor ::= Name typeExpr*
    fn constructor(self: *Parser) !?Statement.Constructor {
        if (self.typeName()) |constructorName| {
            var args = std.ArrayList(*type_inference.Type).init(self.allocator);
            errdefer {
                for (args.items) |arg| {
                    arg.deinit(self.allocator);
                }
                args.deinit();
            }
            var region: utils.Region = .{ .start = 0, .end = 0 };
            while (self.peekToken().type == .Identifier or self.peekToken().type == .LeftParen) {
                try args.append(try self.typeExpr(&region, true, 1));
            }
            return .{ .name = constructorName, .args = args };
        }
        return null;
    }

    // typeStatement ::= type typeName typeVar* = constructor (| constructor)*
    fn typeStatement(self: *Parser) !Statement {
        _ = try self.getToken();

        const name = try self.expectToken(.Identifier);
        if (!std.ascii.isUpper(name.lexeme[0])) {
            try self.errs.errorAt(name.start, name.end, "Types must start with uppercase letters.", .{});
            return error.UnexpectedToken;
        } else if (builtinTypes.has(name.lexeme)) {
            try self.errs.errorAt(name.start, name.end, "The Type {s} is a builtin type.", .{name.lexeme});
            return error.UnexpectedToken;
        } else if (self.algorithmJ.composite.contains(name.lexeme)) {
            try self.errs.errorAt(name.start, name.end, "The Type {s} has already been declared.", .{name.lexeme});
            return error.UnexpectedToken;
        } else if (std.mem.eql(u8, name.lexeme, "Number")) {
            try self.errs.errorAt(name.start, name.end, "A Type can't be named 'Number'.", .{});
            return error.UnexpectedToken;
        }

        self.typeVarMap = .init(self.allocator);
        defer {
            var iterator = self.typeVarMap.?.valueIterator();
            while (iterator.next()) |value| {
                value.*.deinit(self.allocator);
            }
            self.typeVarMap.?.deinit();
            self.typeVarMap = null;
        }

        var numTypeVars: usize = 0;
        var typeArgs = std.ArrayList(*type_inference.Type).init(self.allocator);
        errdefer {
            for (typeArgs.items) |t| {
                t.deinit(self.allocator);
            }
            typeArgs.deinit();
        }

        while (self.typeVar()) |typeVarArg| {
            numTypeVars += 1;
            if (self.typeVarMap.?.contains(typeVarArg.lexeme)) {
                try self.errs.errorAt(typeVarArg.start, typeVarArg.end, "Type Variable already declared.", .{});
                return error.UnexpectedToken;
            }
            const tV = try type_inference.Type.init(self.allocator);
            tV.* = self.algorithmJ.newVarT();
            errdefer tV.deinit(self.allocator);
            try self.typeVarMap.?.put(typeVarArg.lexeme, tV);
            try typeArgs.append(tV);
            tV.rc += 1;
        }

        try self.algorithmJ.composite.put(name.lexeme, .{
            .numVars = numTypeVars,
            .constructors = null,
            .compositeType = null,
        });

        _ = try self.expectToken(.Equal);

        var constructors = std.ArrayList(Statement.Constructor).init(self.allocator);
        errdefer {
            for (constructors.items) |c| {
                for (c.args.items) |arg| {
                    arg.deinit(self.allocator);
                }
                c.args.deinit();
            }
            constructors.deinit();
        }
        var constructorMap = std.StringArrayHashMap(bool).init(self.allocator);
        errdefer constructorMap.deinit();
        var c = try self.constructor();
        if (c) |constructor_| {
            try constructors.append(constructor_);
            try constructorMap.put(constructor_.name.lexeme, false);
        } else {
            try self.errs.errorAt(
                self.peekToken().start,
                self.peekToken().end,
                "Expected the name of a constructor.",
                .{},
            );
            return error.UnexpectedToken;
        }
        while (self.peekToken().type == .VBar) {
            _ = try self.getToken();
            c = try self.constructor();
            if (c) |constructor_| {
                try constructors.append(constructor_);
                try constructorMap.put(constructor_.name.lexeme, false);
            } else {
                try self.errs.errorAt(
                    self.peekToken().start,
                    self.peekToken().end,
                    "Expected the name of a constructor.",
                    .{},
                );
                return error.UnexpectedToken;
            }
        }

        var compositeType = try type_inference.Type.init(self.allocator);
        compositeType.data = .{ .composite = .{
            .name = name.lexeme,
            .args = typeArgs,
        } };
        compositeType.rc += 1;

        self.algorithmJ.composite.getPtr(name.lexeme).?.constructors = constructorMap;
        self.algorithmJ.composite.getPtr(name.lexeme).?.compositeType = compositeType;

        return .{ .type = .{
            .name = name,
            .compositeType = compositeType,
            .constructors = constructors,
        } };
    }

    // main loop as defined by Vaughan R. Pratt in
    // "Top Down Operator Precedence"
    // rbp is right binding power
    fn expression(self: *Parser, rbp: Precedence) !*AST {
        // nud stands for null denotation
        const nud = self.getNud();
        const n = nud orelse {
            try self.errs.errorAt(
                self.peekToken().start,
                self.peekToken().end,
                "{s} can't be at the beginning of an expression.",
                .{token.formatTokenType(self.peekToken().type)},
            );
            return error.InvalidPrefix;
        };
        var left = try n(self);
        errdefer left.deinit(self.allocator);
        // led stands for left denotation
        var led = self.getLed();
        // lbp is left binding power
        var c, var lbp = led orelse return left;
        while (rbp < lbp) {
            left = try c(self, left);
            led = self.getLed();
            c, lbp = led orelse return left;
        }
        return left;
    }

    // Terminals
    // =========

    // intConstant ::= IntLiteral
    fn intLiteral(self: *Parser) !*AST {
        const t = try self.getToken();
        const ast = try self.allocator.create(AST);
        ast.* = .{ .intConstant = .{ .token = t, .value = try std.fmt.parseInt(i64, t.lexeme, 10) } };
        return ast;
    }

    // floatConstant ::= FloatLiteral
    fn floatLiteral(self: *Parser) !*AST {
        const t = try self.getToken();
        const ast = try self.allocator.create(AST);
        ast.* = .{ .floatConstant = .{ .token = t, .value = try std.fmt.parseFloat(f64, t.lexeme) } };
        return ast;
    }

    // boolConstant ::= BoolLiteral
    fn boolLiteral(self: *Parser) !*AST {
        const t = try self.getToken();
        const ast = try self.allocator.create(AST);
        ast.* = .{ .boolConstant = .{
            .token = t,
            .value = std.mem.eql(u8, t.lexeme, "True"),
        } };
        return ast;
    }

    // charConstant ::= charLiteral
    fn charLiteral(self: *Parser) !*AST {
        const t = try self.getToken();
        var value = t.lexeme[0];
        if (t.lexeme[0] == '\\') {
            value = switch (t.lexeme[1]) {
                'n' => '\n',
                'r' => '\r',
                't' => '\t',
                '\'' => '\'',
                '\"' => '\"',
                '\\' => '\\',
                else => {
                    try self.errs.errorAt(t.start + 1, t.start + 2, "Invalid escape code '{s}'.", .{t.lexeme});
                    return error.UnexpectedToken;
                },
            };
        }
        const ast = try self.allocator.create(AST);
        ast.* = .{ .charConstant = .{
            .token = t,
            .value = value,
        } };
        return ast;
    }

    // identifier ::= Identifier
    fn identifier(self: *Parser) !*AST {
        const t = try self.getToken();
        const ast = try self.allocator.create(AST);
        ast.* = .{ .identifier = .{ .token = t } };
        return ast;
    }

    // Non-terminal prefix "operators"
    // ===============================

    // A string is syntax sugar for a list of chars
    fn stringLiteral(self: *Parser) !*AST {
        const t = try self.getToken();
        var chars = std.ArrayList(*AST).init(self.allocator);
        errdefer {
            for (chars.items) |char| {
                char.deinit(self.allocator);
            }
            chars.deinit();
        }
        var i: usize = 1;
        while (i < t.lexeme.len - 1) : (i += 1) {
            var value = t.lexeme[i];
            var len: usize = 1;
            if (t.lexeme[i] == '\\') {
                len += 1;
                value = switch (t.lexeme[i + 1]) {
                    'n' => '\n',
                    'r' => '\r',
                    't' => '\t',
                    '\'' => '\'',
                    '\"' => '\"',
                    else => {
                        try self.errs.errorAt(t.start + 1, t.start + 2, "Invalid escape code '{s}'.", .{t.lexeme});
                        return error.UnexpectedToken;
                    },
                };
            }
            const charLit = try self.allocator.create(AST);
            charLit.* = .{ .charConstant = .{
                .token = .{
                    .start = t.start + i,
                    .end = t.start + i + len,
                    .lexeme = t.lexeme[i .. i + len],
                    .type = .CharLiteral,
                },
                .value = value,
            } };
            try chars.append(charLit);
            i += len - 1;
        }
        const stringList = try self.allocator.create(AST);
        stringList.* = .{ .list = .{ .start = t.start, .end = t.end, .values = chars } };
        return stringList;
    }

    // brackets ::= ( expression )
    fn brackets(self: *Parser) !*AST {
        // Must be a left paren
        _ = try self.getToken();
        const expr = try self.expression(0);
        errdefer expr.deinit(self.allocator);
        // Right paren can be ignored.
        _ = try self.expectToken(.RightParen);
        return expr;
    }

    // letExpression ::= Let Identifier Equal expr In expr
    fn letExpression(self: *Parser) !*AST {
        const start = self.peekToken().start;
        _ = try self.getToken();

        const name = try self.expectToken(.Identifier);

        var typeAnnotation: ?TypeAnnotation = null;
        errdefer {
            if (typeAnnotation) |tA| {
                tA.type.deinit(self.allocator);
            }
        }
        if (self.peekToken().type == .Colon) {
            _ = try self.getToken();
            var typeRegion: utils.Region = .{ .start = 0, .end = 0 };
            self.typeVarMap = .init(self.allocator);
            defer {
                var iterator = self.typeVarMap.?.valueIterator();
                while (iterator.next()) |value| {
                    value.*.deinit(self.allocator);
                }
                self.typeVarMap.?.deinit();
                self.typeVarMap = null;
            }
            const t = try self.constrainedType(&typeRegion, false, 0);
            typeAnnotation = .{ .type = t, .region = typeRegion };
        }

        _ = try self.expectToken(.Equal);

        const be = try self.expression(0);
        errdefer be.deinit(self.allocator);

        _ = try self.expectToken(.In);

        const in = try self.expression(0);
        errdefer in.deinit(self.allocator);

        const letExpr = try self.allocator.create(AST);
        letExpr.* = .{ .let = .{
            .start = start,
            .name = name,
            .be = be,
            .in = in,
            .type = typeAnnotation,
        } };
        return letExpr;
    }

    // lambda ::= Lambda Identifier (Colon constrainedType)? Dot Expr
    fn lambda(self: *Parser) !*AST {
        const start = self.peekToken().start;
        _ = try self.getToken();

        const argname = try self.expectToken(.Identifier);

        var argType: ?TypeAnnotation = null;
        errdefer {
            if (argType) |aT| {
                aT.type.deinit(self.allocator);
            }
        }
        if (self.peekToken().type == .Colon) {
            _ = try self.getToken();
            var typeRegion: utils.Region = .{ .start = 0, .end = 0 };
            self.typeVarMap = .init(self.allocator);
            defer {
                var iterator = self.typeVarMap.?.valueIterator();
                while (iterator.next()) |value| {
                    value.*.deinit(self.allocator);
                }
                self.typeVarMap.?.deinit();
                self.typeVarMap = null;
            }
            const t = try self.constrainedType(&typeRegion, false, 0);
            argType = .{ .type = t, .region = typeRegion };
        }

        _ = try self.expectToken(.Dot);

        const expr = try self.expression(0);
        errdefer expr.deinit(self.allocator);

        const lambdaExpr = try self.allocator.create(AST);
        lambdaExpr.* = .{ .lambda = .{
            .start = start,
            .argname = argname,
            .expr = expr,
            .encloses = null,
            .argType = argType,
        } };

        return lambdaExpr;
    }

    // if ::= If expression Then expression Else expression
    fn ifExpr(self: *Parser) !*AST {
        const start = (try self.getToken()).start;

        const predicate = try self.expression(0);
        errdefer predicate.deinit(self.allocator);

        _ = try self.expectToken(.Then);

        const thenExpr = try self.expression(0);
        errdefer thenExpr.deinit(self.allocator);

        _ = try self.expectToken(.Else);

        const elseExpr = try self.expression(0);
        errdefer elseExpr.deinit(self.allocator);

        const ifAST = try self.allocator.create(AST);
        ifAST.* = .{ .ifExpr = .{
            .start = start,
            .predicate = predicate,
            .thenExpr = thenExpr,
            .elseExpr = elseExpr,
        } };
        return ifAST;
    }

    // A pattern consists of the constructor and variable names for the values
    fn pattern(self: *Parser) !Pattern {
        if (self.peekToken().type == .Identifier) {
            const constructorName = try self.getToken();
            var args = std.ArrayList(token.Token).init(self.allocator);
            while (self.peekToken().type == .Identifier) {
                try args.append(try self.getToken());
            }
            return .{
                .name = constructorName,
                .values = args,
            };
        }
        try self.errs.errorAt(
            self.peekToken().start,
            self.peekToken().end,
            "Expected a pattern.",
            .{},
        );
        return error.UnexpectedToken;
    }

    // caseExpr ::= Case expression Of pattern DoubleArrow expression (Pipe pattern DoubleArrow expression)*
    fn caseExpr(self: *Parser) !*AST {
        const start = (try self.getToken()).start;
        const value = try self.expression(0);
        errdefer value.deinit(self.allocator);
        _ = try self.expectToken(.Of);
        var patterns = std.ArrayList(Pattern).init(self.allocator);
        errdefer {
            for (patterns.items) |*currentPattern| {
                currentPattern.deinit();
            }
            patterns.deinit();
        }
        var bodies = std.ArrayList(*AST).init(self.allocator);
        errdefer {
            for (bodies.items) |body| {
                body.deinit(self.allocator);
            }
            bodies.deinit();
        }
        try patterns.append(try self.pattern());
        _ = try self.expectToken(.DoubleArrow);
        try bodies.append(try self.expression(0));
        while (self.peekToken().type == .VBar) {
            _ = try self.getToken();
            try patterns.append(try self.pattern());
            _ = try self.expectToken(.DoubleArrow);
            try bodies.append(try self.expression(0));
        }
        const caseAST = try self.allocator.create(AST);
        caseAST.* = .{ .case = .{
            .start = start,
            .value = value,
            .patterns = patterns,
            .bodies = bodies,
        } };
        return caseAST;
    }

    // prefixOp ::= Operator
    fn prefixOp(self: *Parser) !*AST {
        const t = try self.getToken();
        const prec: usize = switch (t.lexeme[0]) {
            '-' => 10,
            '!' => 0,
            else => undefined,
        };
        const expr = try self.expression(prec);
        errdefer expr.deinit(self.allocator);
        const prefixOpAST = try self.allocator.create(AST);
        prefixOpAST.* = .{ .prefixOp = .{ .token = t, .expr = expr } };
        return prefixOpAST;
    }

    // list ::= LeftBracket (expression (Comma expression)* Comma?)? RightBracket
    fn list(self: *Parser) !*AST {
        const start = (try self.getToken()).start;

        var exprs = std.ArrayList(*AST).init(self.allocator);
        errdefer {
            for (exprs.items) |expr| {
                expr.deinit(self.allocator);
            }
            exprs.deinit();
        }

        {
            if (self.peekToken().type != .RightBracket) {
                try exprs.append(try self.expression(0));
            }

            while (self.peekToken().type == .Comma) {
                _ = try self.getToken();
                if (self.peekToken().type != .RightBracket) {
                    try exprs.append(try self.expression(0));
                }
            }
        }

        const end = (try self.expectToken(.RightBracket)).end;

        const listAst = try self.allocator.create(AST);

        listAst.* = .{ .list = .{
            .start = start,
            .end = end,
            .values = exprs,
        } };

        return listAst;
    }

    // Get rule in prefix position
    fn getNud(self: *Parser) ?PrefixParseFn {
        return switch (self.peekToken().type) {
            token.TokenType.IntLiteral => intLiteral,
            token.TokenType.FloatLiteral => floatLiteral,
            token.TokenType.BoolLiteral => boolLiteral,
            token.TokenType.CharLiteral => charLiteral,
            token.TokenType.StringLiteral => stringLiteral,
            token.TokenType.LeftParen => brackets,
            token.TokenType.Identifier => identifier,
            token.TokenType.Let => letExpression,
            token.TokenType.Lambda => lambda,
            token.TokenType.If => ifExpr,
            token.TokenType.Case => caseExpr,
            token.TokenType.LeftBracket => list,
            token.TokenType.Operator => {
                if (self.peekToken().lexeme.len == 1 and
                    (self.peekToken().lexeme[0] == '-' or self.peekToken().lexeme[0] == '!'))
                {
                    return prefixOp;
                }
                return null;
            },
            else => null,
        };
    }

    // operator ::= Operator
    fn operator(self: *Parser, left: *AST) !*AST {
        const t = try self.getToken();
        const prec: usize = switch (t.lexeme[0]) {
            ';' => 0,
            // or and and
            'o' => 20,
            'a' => 30,
            '<', '>', '!', '=' => 40,
            '+', '-' => 50,
            '*', '/' => 60,
            // Exponentiation associates to the right
            '^' => 60,
            else => undefined,
        };
        const right = try self.expression(prec);
        errdefer right.deinit(self.allocator);
        const astOp = try self.allocator.create(AST);
        astOp.* = .{ .operator = .{
            .token = t,
            .left = left,
            .right = right,
        } };
        return astOp;
    }

    // call ::= expression expression
    fn call(self: *Parser, left: *AST) !*AST {
        // A call is left associative, therefore also max precedence
        const expr = try self.expression(std.math.maxInt(Precedence));
        errdefer expr.deinit(self.allocator);
        const astCall = try self.allocator.create(AST);
        astCall.* = .{ .call = .{ .function = left, .arg = expr } };
        return astCall;
    }

    // get rule for token in the middle of expression (infix, postfix or mixfix)
    fn getLed(self: *Parser) ?InfixParseRule {
        return switch (self.peekToken().type) {
            .Or => {
                return .{ operator, 20 };
            },
            .And => {
                return .{ operator, 30 };
            },
            .Operator => {
                if (self.peekToken().lexeme.len == 1 and self.peekToken().lexeme[0] == '!') {
                    return .{ call, std.math.maxInt(Precedence) };
                }
                const prec: usize = switch (self.peekToken().lexeme[0]) {
                    ';' => 10,
                    '<', '>', '=', '!' => 40,
                    '+', '-' => 50,
                    '*', '/' => 60,
                    '^' => 70,
                    else => undefined,
                };
                return .{ operator, prec };
            },
            else => {
                // Special rule: Values next to each other leads to function call.
                // Function call has highest possible precedence
                if (self.getNud()) |_| {
                    return .{ call, std.math.maxInt(Precedence) };
                } else {
                    return null;
                }
            },
        };
    }

    // Expect a token of a given type
    fn expectToken(self: *Parser, tt: token.TokenType) !token.Token {
        if (self.peekToken().type != tt) {
            try self.errs.errorAt(
                self.peekToken().start,
                self.peekToken().end,
                "Expected {s}, got: {s}",
                .{ token.formatTokenType(tt), token.formatTokenType(self.peekToken().type) },
            );
            return error.UnexpectedToken;
        } else {
            return self.getToken();
        }
    }

    // Helper function for getting the next token
    // self.next serves as a buffer to enable peeking at the token
    // All access of tokens must go through this function
    fn getToken(self: *Parser) !token.Token {
        const current = self.next;
        // Don't always get next token, since it might be the end
        if (current.type != .EOF) {
            self.next = try self.lexer.getToken();
        } else {
            self.atEnd = true;
        }
        return current;
    }

    fn peekToken(self: *Parser) token.Token {
        return self.next;
    }
};
