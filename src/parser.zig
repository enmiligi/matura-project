const std = @import("std");
const token = @import("./token.zig");
const lexer = @import("./lexer.zig");
const AST = @import("./ast.zig").AST;
const errors = @import("./errors.zig");

const Precedence = usize;
const PrefixParseFn = *const fn (self: *Parser) anyerror!*AST;
const InfixParseFn = *const fn (self: *Parser, left: *AST) anyerror!*AST;
const InfixParseRule = struct { InfixParseFn, Precedence };

const nullToken = token.Token{ .start = 0, .end = 0, .lexeme = "", .type = .Identifier };

pub const ParserError = error{
    InvalidPrefix,
    UnexpectedToken,
};

pub const Parser = struct {
    allocator: std.mem.Allocator,
    lexer: *lexer.Lexer,
    next: token.Token,
    atEnd: bool = false,
    errs: *errors.Errors,

    // Initialize parser and Lexer
    pub fn init(allocator: std.mem.Allocator, source: []const u8, errs: *errors.Errors) !Parser {
        var l = try allocator.create(lexer.Lexer);
        l.* = .{ .source = source, .errs = errs };
        return .{
            .allocator = allocator,
            .lexer = l,
            .next = try l.getToken(),
            .errs = errs,
        };
    }

    pub fn deinit(self: *Parser) void {
        self.allocator.destroy(self.lexer);
    }

    // At the moment everything is an expression
    pub fn parse(self: *Parser) !*AST {
        return self.expression(0);
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

    // let ::= Let Identifier Equal expr In expr
    fn let(self: *Parser) !*AST {
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

        _ = try self.expectToken(.Dot);

        const expr = try self.expression(0);
        errdefer expr.deinit(self.allocator);

        const lambdaExpr = try self.allocator.create(AST);
        lambdaExpr.* = .{ .lambda = .{
            .start = start,
            .argname = argname,
            .expr = expr,
        } };

        return lambdaExpr;
    }

    // Get rule in prefix position
    fn getNud(self: *Parser) ?PrefixParseFn {
        return switch (self.peekToken().type) {
            token.TokenType.IntLiteral => intLiteral,
            token.TokenType.FloatLiteral => floatLiteral,
            token.TokenType.LeftParen => brackets,
            token.TokenType.Identifier => identifier,
            token.TokenType.Let => let,
            token.TokenType.Lambda => lambda,
            else => null,
        };
    }

    fn operator(self: *Parser, left: *AST) !*AST {
        errdefer left.deinit(self.allocator);
        const t = try self.getToken();
        const prec: usize = switch (t.lexeme[0]) {
            '+', '-' => 10,
            '*', '/' => 20,
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
                const prec: usize = switch (self.peekToken().lexeme[0]) {
                    '+', '-' => 10,
                    '*', '/' => 20,
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
