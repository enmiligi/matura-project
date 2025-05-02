const std = @import("std");
const token = @import("./token.zig");
const lexer = @import("./lexer.zig");
const AST = @import("./ast.zig").AST;
const TypeAnnotation = @import("./ast.zig").TypeAnnotation;
const Statement = @import("./ast.zig").Statement;
const errors = @import("./errors.zig");
const type_inference = @import("./type_inference.zig");

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
        source: []const u8,
        errs: *errors.Errors,
        algorithmJ: *type_inference.AlgorithmJ,
    ) !Parser {
        var l = try allocator.create(lexer.Lexer);
        l.* = .{ .source = source, .errs = errs };
        return .{
            .allocator = allocator,
            .lexer = l,
            .next = try l.getToken(),
            .errs = errs,
            .algorithmJ = algorithmJ,
            .typeVarMap = null,
        };
    }

    pub fn deinit(self: *Parser) void {
        self.allocator.destroy(self.lexer);
    }

    pub fn parseToEnd(self: *Parser) !*AST {
        const res = try self.parse();
        if (!(self.peekToken().type == .EOF)) {
            res.deinit(self.allocator);
            try self.errs.errorAt(self.peekToken().start, self.peekToken().end, "Unexpected token", .{});
            return error.UnexpectedToken;
        }
        return res;
    }

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

    pub fn typeExpr(self: *Parser, region: *errors.Region, declaredVars: bool, callPrecedence: bool) !*type_inference.Type {
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
            region.start = start;
            _ = try self.expectToken(.DoubleArrow);
        }
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
            } else if (self.algorithmJ.composite.get(tN.lexeme)) |numArgs| {
                if (numArgs > 0 and !callPrecedence) {
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
                while (i < numArgs) : (i += 1) {
                    var _region: errors.Region = .{ .start = 0, .end = 0 };
                    const arg = try self.typeExpr(&_region, declaredVars, false);
                    try args.append(arg);
                }
                currentType.data = .{ .composite = .{ .name = tN.lexeme, .args = args } };
            } else {
                return error.InvalidType;
            }
        } else if (self.peekToken().type == .LeftParen) {
            const t = try self.getToken();
            if (region.start == 0) {
                region.start = t.start;
            }
            var r: errors.Region = .{ .start = 0, .end = 0 };
            currentType = try self.typeExpr(&r, declaredVars, true);
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
        if (self.peekToken().type == .Arrow) {
            _ = try self.getToken();
            errdefer currentType.deinit(self.allocator);
            var innerRegion: errors.Region = .{ .start = 0, .end = 0 };
            const returnType = try self.typeExpr(&innerRegion, declaredVars, callPrecedence);
            region.end = innerRegion.end;
            errdefer returnType.deinit(self.allocator);
            const argumentType = currentType;
            currentType = try type_inference.Type.init(self.allocator);
            currentType.data = .{ .function = .{
                .from = argumentType,
                .to = returnType,
            } };
        }
        if (callPrecedence and (self.peekToken().type == .LeftParen or self.peekToken().type == .Identifier)) {
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
                    _ = try self.expectToken(.NewStatement);
                },
            }
        }
        return statements;
    }

    // At the moment, the only statement is let
    fn statement(self: *Parser) !Statement {
        switch (self.peekToken().type) {
            .Let => {
                return self.letStatement();
            },
            .Type => {
                return self.typeStatement();
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
            var typeRegion: errors.Region = .{ .start = 0, .end = 0 };
            self.typeVarMap = .init(self.allocator);
            defer {
                var iterator = self.typeVarMap.?.valueIterator();
                while (iterator.next()) |value| {
                    value.*.deinit(self.allocator);
                }
                self.typeVarMap.?.deinit();
                self.typeVarMap = null;
            }
            const t = try self.typeExpr(&typeRegion, false, true);
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

    fn constructor(self: *Parser) !?Statement.Constructor {
        if (self.typeName()) |constructorName| {
            var args = std.ArrayList(*type_inference.Type).init(self.allocator);
            errdefer {
                for (args.items) |arg| {
                    arg.deinit(self.allocator);
                }
                args.deinit();
            }
            var region: errors.Region = .{ .start = 0, .end = 0 };
            while (self.peekToken().type == .Identifier or self.peekToken().type == .LeftParen) {
                try args.append(try self.typeExpr(&region, true, false));
            }
            return .{ .name = constructorName, .args = args };
        }
        return null;
    }

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

        try self.algorithmJ.composite.put(name.lexeme, numTypeVars);

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
        var c = try self.constructor();
        if (c) |constructor_| {
            try constructors.append(constructor_);
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
                "This can't be used at the beginning of an expression.",
                .{},
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

    fn boolLiteral(self: *Parser) !*AST {
        const t = try self.getToken();
        const ast = try self.allocator.create(AST);
        ast.* = .{ .boolConstant = .{
            .token = t,
            .value = std.mem.eql(u8, t.lexeme, "True"),
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
            var typeRegion: errors.Region = .{ .start = 0, .end = 0 };
            self.typeVarMap = .init(self.allocator);
            defer {
                var iterator = self.typeVarMap.?.valueIterator();
                while (iterator.next()) |value| {
                    value.*.deinit(self.allocator);
                }
                self.typeVarMap.?.deinit();
                self.typeVarMap = null;
            }
            const t = try self.typeExpr(&typeRegion, false, true);
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

    // lambda ::= Lambda Identifier Dot Expr
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
            var typeRegion: errors.Region = .{ .start = 0, .end = 0 };
            self.typeVarMap = .init(self.allocator);
            defer {
                var iterator = self.typeVarMap.?.valueIterator();
                while (iterator.next()) |value| {
                    value.*.deinit(self.allocator);
                }
                self.typeVarMap.?.deinit();
                self.typeVarMap = null;
            }
            const t = try self.typeExpr(&typeRegion, false, true);
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

    // Get rule in prefix position
    fn getNud(self: *Parser) ?PrefixParseFn {
        return switch (self.peekToken().type) {
            token.TokenType.IntLiteral => intLiteral,
            token.TokenType.FloatLiteral => floatLiteral,
            token.TokenType.BoolLiteral => boolLiteral,
            token.TokenType.LeftParen => brackets,
            token.TokenType.Identifier => identifier,
            token.TokenType.Let => letExpression,
            token.TokenType.Lambda => lambda,
            token.TokenType.If => ifExpr,
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

    fn operator(self: *Parser, left: *AST) !*AST {
        errdefer left.deinit(self.allocator);
        const t = try self.getToken();
        const prec: usize = switch (t.lexeme[0]) {
            '<', '>', '!', '=' => 10,
            '+', '-' => 20,
            '*', '/' => 30,
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
            .Operator => {
                if (self.peekToken().lexeme.len == 1 and self.peekToken().lexeme[0] == '!') {
                    return .{ call, std.math.maxInt(Precedence) };
                }
                const prec: usize = switch (self.peekToken().lexeme[0]) {
                    '<', '>', '=', '!' => 10,
                    '+', '-' => 20,
                    '*', '/' => 30,
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
