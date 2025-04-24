const std = @import("std");
const token = @import("./token.zig");
const lexer = @import("./lexer.zig");
const AST = @import("./ast.zig").AST;
const Statement = @import("./ast.zig").Statement;
const errors = @import("./errors.zig");
const type_inference = @import("./type_inference.zig");

const Precedence = usize;
const PrefixParseFn = *const fn (self: *Parser) anyerror!*AST;
const InfixParseFn = *const fn (self: *Parser, left: *AST) anyerror!*AST;
const InfixParseRule = struct { InfixParseFn, Precedence };

const nullToken = token.Token{ .start = 0, .end = 0, .lexeme = "", .type = .Identifier };

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

    pub fn typeExpr(self: *Parser, region: *errors.Region) !*type_inference.Type {
        _ = try self.getToken();
        var currentType: *type_inference.Type = undefined;
        if (self.typeVar()) |typeVarName| {
            region.start = typeVarName.start;
            region.end = typeVarName.end;
            if (self.typeVarMap.?.get(typeVarName.lexeme)) |tV| {
                currentType = tV;
                tV.rc += 1;
            } else {
                var newTypeVar = try type_inference.Type.init(self.allocator);
                errdefer newTypeVar.deinit(self.allocator);
                newTypeVar.* = self.algorithmJ.newVarT();
                try self.typeVarMap.?.put(typeVarName.lexeme, newTypeVar);
                currentType = newTypeVar;
            }
        } else if (self.typeName()) |tN| {
            region.start = tN.start;
            region.end = tN.end;
            currentType = try type_inference.Type.init(self.allocator);
            errdefer currentType.deinit(self.allocator);
            if (std.mem.eql(u8, tN.lexeme, "Int")) {
                currentType.data = .{ .primitive = .Int };
            } else if (std.mem.eql(u8, tN.lexeme, "Float")) {
                currentType.data = .{ .primitive = .Float };
            } else if (std.mem.eql(u8, tN.lexeme, "Bool")) {
                currentType.data = .{ .primitive = .Bool };
            } else {
                return error.InvalidType;
            }
        } else if (self.peekToken().type == .LeftParen) {
            region.start = (try self.getToken()).start;
            var r: errors.Region = .{ .start = 0, .end = 0 };
            currentType = try self.typeExpr(&r);
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
            errdefer currentType.deinit(self.allocator);
            var _region: errors.Region = .{ .start = 0, .end = 0 };
            const returnType = try self.typeExpr(&_region);
            errdefer returnType.deinit(self.allocator);
            const argumentType = currentType;
            currentType = try type_inference.Type.init(self.allocator);
            currentType.data = .{ .function = .{
                .from = argumentType,
                .to = returnType,
            } };
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
        const createdTypeVarMap = self.typeVarMap == null;
        if (createdTypeVarMap) {
            self.typeVarMap = .init(self.allocator);
        }
        defer if (createdTypeVarMap) {
            self.typeVarMap.?.deinit();
            self.typeVarMap = null;
        };
        switch (self.peekToken().type) {
            .Let => {
                return self.letStatement();
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
        const start = self.peekToken().start;
        _ = try self.getToken();

        const name = try self.expectToken(.Identifier);

        _ = try self.expectToken(.Equal);

        const be = try self.expression(0);
        errdefer be.deinit(self.allocator);

        const letStmt: Statement = .{ .let = .{
            .start = start,
            .name = name,
            .be = be,
        } };
        return letStmt;
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
        } };
        return letExpr;
    }

    // lambda ::= Lambda Identifier Dot Expr
    fn lambda(self: *Parser) !*AST {
        const start = self.peekToken().start;
        _ = try self.getToken();

        const argname = try self.expectToken(.Identifier);

        var argType: ?*type_inference.Type = null;
        var typeRegion: ?errors.Region = null;
        errdefer {
            if (argType) |aT| {
                aT.deinit(self.allocator);
            }
        }
        if (self.peekToken().type == .Colon) {
            typeRegion = .{ .start = 0, .end = 0 };
            argType = try self.typeExpr(&typeRegion.?);
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
            .typeRegion = typeRegion,
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

test "parser parses lets" {
    const allocator = std.testing.allocator;
    const code = "let x = 5 in x\n\t";
    var p = try Parser.init(allocator, code);
    defer p.deinit();
    const ast = try p.parse();
    defer ast.deinit(allocator);

    var let: *AST = undefined;
    var be: *AST = undefined;
    var in: *AST = undefined;
    {
        let = try allocator.create(AST);
        errdefer let.deinit(allocator);
        be = try allocator.create(AST);
        errdefer be.deinit(allocator);
        in = try allocator.create(AST);
    }

    be.* = .{
        .intConstant = .{
            .value = 5,
            .token = .{
                .start = 8,
                .end = 9,
                .lexeme = "5",
                .type = .IntLiteral,
            },
        },
    };
    in.* = .{ .identifier = .{ .token = .{
        .start = 13,
        .end = 14,
        .lexeme = "x",
        .type = .Identifier,
    } } };
    let.* = .{ .let = .{
        .name = .{
            .start = 4,
            .end = 5,
            .lexeme = "x",
            .type = .Identifier,
        },
        .in = in,
        .be = be,
    } };
    defer let.deinit(allocator);

    try std.testing.expectEqualDeep(let, ast);
}

test "parser parses lambdas" {
    const allocator = std.testing.allocator;
    const code = "lambda x. x\n";
    var p = try Parser.init(allocator, code);
    defer p.deinit();
    const ast = try p.parse();
    defer ast.deinit(allocator);

    var x: *AST = undefined;
    var lambda: *AST = undefined;
    {
        x = try allocator.create(AST);
        errdefer x.deinit(allocator);
        lambda = try allocator.create(AST);
    }
    x.* = .{ .identifier = .{ .token = .{
        .start = 10,
        .end = 11,
        .lexeme = "x",
        .type = .Identifier,
    } } };

    lambda.* = .{ .lambda = .{ .argname = .{
        .start = 7,
        .end = 8,
        .lexeme = "x",
        .type = .Identifier,
    }, .expr = x } };
    defer lambda.deinit(allocator);

    try std.testing.expectEqualDeep(lambda, ast);
}

test "parser parses numbers and calls correctly" {
    const allocator = std.testing.allocator;
    const code = "e1 (5.2 5 e2)\n";
    var p = try Parser.init(allocator, code);
    defer p.deinit();
    const ast = try p.parse();
    defer ast.deinit(allocator);
    var float: *AST = undefined;
    var int: *AST = undefined;
    var e1: *AST = undefined;
    var e2: *AST = undefined;
    var call1: *AST = undefined;
    var call2: *AST = undefined;
    var call3: *AST = undefined;
    {
        float = try allocator.create(AST);
        errdefer allocator.destroy(float);
        int = try allocator.create(AST);
        errdefer allocator.destroy(int);
        e1 = try allocator.create(AST);
        errdefer allocator.destroy(e1);
        e2 = try allocator.create(AST);
        errdefer allocator.destroy(e2);
        call1 = try allocator.create(AST);
        errdefer allocator.destroy(call1);
        call2 = try allocator.create(AST);
        errdefer allocator.destroy(call2);
        call3 = try allocator.create(AST);
    }
    float.* = .{
        .floatConstant = .{
            .value = 5.2,
            .token = .{ .start = 4, .end = 7, .lexeme = "5.2", .type = .FloatLiteral },
        },
    };
    int.* = .{
        .intConstant = .{
            .value = 5,
            .token = .{ .start = 8, .end = 9, .lexeme = "5", .type = .IntLiteral },
        },
    };
    e1.* = .{ .identifier = .{ .token = .{
        .start = 0,
        .end = 2,
        .lexeme = "e1",
        .type = .Identifier,
    } } };
    e2.* = .{ .identifier = .{ .token = .{
        .start = 10,
        .end = 12,
        .lexeme = "e2",
        .type = .Identifier,
    } } };
    call2.* = .{ .call = .{
        .function = float,
        .arg = int,
    } };
    call3.* = .{ .call = .{
        .function = call2,
        .arg = e2,
    } };
    call1.* = .{ .call = .{
        .function = e1,
        .arg = call3,
    } };
    defer call1.deinit(allocator);
    try std.testing.expectEqualDeep(call1, ast);
}

test "parser errors on invalid source code" {
    const allocator = std.testing.allocator;
    const unexpectedTokenSource = "abc ( d in\n";
    var p = try Parser.init(allocator, unexpectedTokenSource);
    defer p.deinit();
    try std.testing.expectError(error.UnexpectedToken, p.parse());

    const invalidPrefixSource = "in abc";
    p.deinit();
    p = try Parser.init(allocator, invalidPrefixSource);
    try std.testing.expectError(error.InvalidPrefix, p.parse());
}

test "parser stops parsing at invalid infix" {
    const invalidInfix = "a in abc";
    var p = try Parser.init(std.testing.allocator, invalidInfix);
    defer p.deinit();
    (try p.parse()).deinit(std.testing.allocator);
    try std.testing.expect(!p.atEnd);
}
