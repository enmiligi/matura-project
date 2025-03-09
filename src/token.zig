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
    Operator,
    // literals
    FloatLiteral,
    IntLiteral,
    // user-defined
    Identifier,
    // End of File
    EOF,
};

pub fn formatTokenType(tt: TokenType) []const u8 {
    const name = switch (tt) {
        .Dot => "a dot",
        .Equal => "an equal sign",
        .FloatLiteral => "a decimal number",
        .Identifier => "an identifier",
        .In => "'in'",
        .IntLiteral => "an integer",
        .Lambda => "'lambda'",
        .LeftParen => "'('",
        .Let => "'let'",
        .RightParen => "')'",
        .EOF => "the end of the file",
        .Operator => "an operator",
    };
    return name;
}

// lexeme = source[start..end]
// end is one past the last char
pub const Token = struct {
    type: TokenType,
    lexeme: []const u8,
    start: usize,
    end: usize,
};
