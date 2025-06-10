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
        .{ "type", .Type },
        .{ "case", .Case },
        .{ "of", .Of },
    });

pub const Lexer = struct {
    source: []const u8,
    tokenStart: usize = 0,
    location: usize = 0,
    errs: *errors.Errors,

    pub fn newSource(self: *Lexer, source: []const u8) void {
        self.source = source;
        self.tokenStart = 0;
        self.location = 0;
    }

    fn isAtEnd(self: *Lexer) bool {
        return self.location == self.source.len;
    }

    // This function generates the next token
    pub fn getToken(self: *Lexer) !token.Token {
        if (self.skipWhitespace()) |newStatement| {
            return newStatement;
        }
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
            if (c == '\'') {
                self.location += 1;
                self.tokenStart += 1;
                if (self.getChar() == '\\') {
                    self.location += 1;
                }
                self.location += 1;
                if (self.location >= self.source.len or self.getChar() != '\'') {
                    try self.errs.errorAt(self.location, self.location, "There should be a ' to end the char.", .{});
                    return error.InvalidChar;
                }
                const t = self.makeToken(.CharLiteral);
                self.location += 1;
                return t;
            }
            if (c == '\"') {
                self.location += 1;
                while (self.location < self.source.len and self.getChar() != '"') {
                    if (self.getChar() == '\\') {
                        self.location += 1;
                    }
                    self.location += 1;
                }
                if (self.location == self.source.len) {
                    try self.errs.errorAt(self.location, self.location, "There should be a \" to end the string.", .{});
                    return error.InvalidChar;
                }
                self.location += 1;
                return self.makeToken(.StringLiteral);
            }
            if (c == '-' and self.source[self.location + 1] == '>') {
                self.location += 2;
                return self.makeToken(.Arrow);
            }
            if (c == '=' or c == '!' or c == '<' or c == '>') {
                return self.doubleOperator();
            }
            const tt: ?token.TokenType = switch (c) {
                '(' => .LeftParen,
                ')' => .RightParen,
                '[' => .LeftBracket,
                ']' => .RightBracket,
                '.' => .Dot,
                ':' => .Colon,
                ',' => .Comma,
                '|' => .VBar,
                '+', '-', '*', '/', ';' => .Operator,
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

    // Make a token at the current location
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

    // Handle operators that consist of 2 characters
    fn doubleOperator(self: *Lexer) !token.Token {
        var tt: token.TokenType = undefined;
        if (self.getChar() == '=') {
            self.location += 1;
            if (self.getChar() == '=') {
                self.location += 1;
                tt = .Operator;
            } else if (self.getChar() == '>') {
                self.location += 1;
                tt = .DoubleArrow;
            } else {
                tt = .Equal;
            }
        } else if (self.getChar() == '!' or self.getChar() == '<' or self.getChar() == '>') {
            self.location += 1;
            if (self.getChar() == '=') {
                self.location += 1;
            }
            tt = .Operator;
        }
        return self.makeToken(tt);
    }

    // identifier ::= alpha (alnum|_)*
    fn identifier(self: *Lexer) token.Token {
        while (self.location != self.source.len and (std.ascii.isAlphanumeric(self.getChar()) or self.getChar() == '_')) {
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

    // Go over all whitespace and create New Statement token if
    // the current line is not indented
    fn skipWhitespace(self: *Lexer) ?token.Token {
        var newLineOccurred = false;
        while (!self.isAtEnd() and std.ascii.isWhitespace(self.getChar())) {
            if (self.getChar() == '\n') {
                newLineOccurred = true;
                self.tokenStart = self.location;
            }
            self.location += 1;
        }
        if (newLineOccurred and self.tokenStart + 1 == self.location) {
            self.tokenStart += 1;
            return self.makeToken(.NewStatement);
        }
        self.tokenStart = self.location;
        return null;
    }

    // Get the current character
    inline fn getChar(self: *Lexer) u8 {
        return self.source[self.location];
    }

    // Get the current token lexeme
    inline fn getTokenString(self: *Lexer) []const u8 {
        return self.source[self.tokenStart..self.location];
    }
};
