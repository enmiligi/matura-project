const std = @import("std");
pub const TokenType = enum {
    // keywords
    In,
    Let,
    Lambda,
    // special characters
    Dot,
    Equal,
    LeftParen,
    RightParen,
    // literals
    FloatLiteral,
    IntLiteral,
    // user-defined
    Identifier,
};

// lexeme = source[start..end]
// end is one past the last char
pub const Token = struct {
    type: TokenType,
    lexeme: []const u8,
    start: usize,
    end: usize,
};
