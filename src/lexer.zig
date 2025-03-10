const std = @import("std");
pub const token = @import("token.zig");
const errors = @import("./errors.zig");

const LexerError = error{
    InvalidChar,
};

// map special identifiers to keyword TokenType
const identifierMap =
    std.StaticStringMap(token.TokenType)
        .initComptime(.{
        .{ "let", .Let },
        .{ "lambda", .Lambda },
        .{ "in", .In },
        .{ "True", .BoolLiteral },
        .{ "False", .BoolLiteral },
        .{ "if", .If },
        .{ "then", .Then },
        .{ "else", .Else },
    });

pub const Lexer = struct {
    source: []const u8,
    tokenStart: usize = 0,
    location: usize = 0,
    errs: *errors.Errors,

    fn isAtEnd(self: *Lexer) bool {
        return self.location == self.source.len;
    }

    // This function generates the next token
    pub fn getToken(self: *Lexer) !token.Token {
        self.skipWhitespace();
        self.tokenStart = self.location;
        if (self.isAtEnd()) return self.makeToken(.EOF);
        const c = self.getChar();
        // Possible Tokens:
        // identifier and keywords: start with alphanumeric
        // numbers: start with digit
        // special characters: single char
        if (std.ascii.isAlphabetic(c)) {
            return self.identifier();
        } else if (std.ascii.isDigit(c)) {
            return self.numberLiteral();
        } else {
            if (c == '=' or c == '!') {
                return self.doubleOperator();
            }
            const tt: ?token.TokenType = switch (c) {
                '(' => .LeftParen,
                ')' => .RightParen,
                '.' => .Dot,
                '+', '-', '*', '/', '<', '>' => .Operator,
                else => null,
            };
            if (tt) |value| {
                self.location += 1;
                return self.makeToken(value);
            } else {
                try self.errs.errorAt(self.location, self.location + 1, "Invalid Character", .{});
                return error.InvalidChar;
            }
        }
    }

    // This is a function mostly meant for testing collecting all generated tokens.
    pub fn collectTokens(self: *Lexer, allocator: std.mem.Allocator) !std.ArrayList(token.Token) {
        var tokens = std.ArrayList(token.Token).init(allocator);
        errdefer tokens.deinit();

        var t = try self.getToken();
        while (!(t.type == .EOF)) {
            try tokens.append(t);
            t = try self.getToken();
        }
        try tokens.append(t);

        return tokens;
    }

    // Tokens have their end one past the last character
    fn makeToken(self: *Lexer, tt: token.TokenType) token.Token {
        const t: token.Token = .{
            .type = tt,
            .start = self.tokenStart,
            .end = self.location,
            .lexeme = self.getTokenString(),
        };
        self.tokenStart = self.location;
        return t;
    }

    fn doubleOperator(self: *Lexer) !token.Token {
        var tt: token.TokenType = undefined;
        if (self.getChar() == '=') {
            self.location += 1;
            if (self.getChar() == '=') {
                self.location += 1;
                tt = .Operator;
            } else {
                tt = .Equal;
            }
        } else if (self.getChar() == '!') {
            self.location += 1;
            if (self.getChar() == '=') {
                self.location += 1;
                tt = .Operator;
            } else {
                return error.InvalidChar;
            }
        }
        return self.makeToken(tt);
    }

    // identifier ::= alpha (alnum|_)*
    fn identifier(self: *Lexer) token.Token {
        while (std.ascii.isAlphanumeric(self.getChar()) or self.getChar() == '_') {
            self.location += 1;
        }
        const tt = identifierMap.get(self.getTokenString()) orelse .Identifier;
        return self.makeToken(tt);
    }

    // int ::= digit+
    // float ::= digit+ '.' digit*
    fn numberLiteral(self: *Lexer) token.Token {
        while (std.ascii.isDigit(self.getChar())) {
            self.location += 1;
        }
        var tt = token.TokenType.IntLiteral;
        if (self.getChar() == '.') {
            self.location += 1;
            while (std.ascii.isDigit(self.getChar())) {
                self.location += 1;
            }
            tt = token.TokenType.FloatLiteral;
        }
        return self.makeToken(tt);
    }

    fn skipWhitespace(self: *Lexer) void {
        while (!self.isAtEnd() and std.ascii.isWhitespace(self.getChar())) {
            self.location += 1;
        }
    }

    fn getChar(self: *Lexer) u8 {
        return self.source[self.location];
    }

    fn getTokenString(self: *Lexer) []const u8 {
        return self.source[self.tokenStart..self.location];
    }
};

// Function meant only for testing
// creates an ArrayList of tokens via a temporary Lexer
fn getTokensForCode(code: []const u8, allocator: std.mem.Allocator) !std.ArrayList(token.Token) {
    var lexer = Lexer{ .source = code };
    const tokens = try lexer.collectTokens(allocator);
    return tokens;
}

test "lexer returns correct tokens" {
    const allocator = std.testing.allocator;
    const expectedTokens: [12]token.Token = .{
        .{ .start = 0, .end = 3, .lexeme = "let", .type = .Let },
        .{ .start = 4, .end = 10, .lexeme = "letter", .type = .Identifier },
        .{ .start = 11, .end = 12, .lexeme = "=", .type = .Equal },
        .{ .start = 13, .end = 14, .lexeme = "5", .type = .IntLiteral },
        .{ .start = 15, .end = 17, .lexeme = "in", .type = .In },
        .{ .start = 18, .end = 19, .lexeme = "(", .type = .LeftParen },
        .{ .start = 19, .end = 25, .lexeme = "lambda", .type = .Lambda },
        .{ .start = 26, .end = 27, .lexeme = "x", .type = .Identifier },
        .{ .start = 27, .end = 28, .lexeme = ".", .type = .Dot },
        .{ .start = 29, .end = 33, .lexeme = "7.65", .type = .FloatLiteral },
        .{ .start = 33, .end = 34, .lexeme = ")", .type = .RightParen },
        .{ .start = 35, .end = 35, .lexeme = "", .type = .EOF },
    };
    const code = "let letter = 5 in (lambda x. 7.65)\n";
    var tokens = try getTokensForCode(code, allocator);
    defer tokens.deinit();

    try std.testing.expectEqualDeep(expectedTokens[0..], tokens.items);
}

test "lexer rejects invalid characters" {
    try std.testing.expectError(
        error.InvalidChar,
        getTokensForCode("let a = 77 in @a", std.testing.allocator),
    );
}
