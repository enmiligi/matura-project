const std = @import("std");
pub const TokenType = enum {
    // keywords
    In,
    Let,
    Lambda,
    If,
    Then,
    Else,
    // special characters
    Dot,
    Equal,
    LeftParen,
    RightParen,
    LeftBracket,
    RightBracket,
    Operator,
    Comma,
    // literals
    FloatLiteral,
    IntLiteral,
    BoolLiteral,
    CharLiteral,
    // user-defined
    Identifier,
    // Newline with no whitespace
    NewStatement,
    // End of File
    EOF,
    // Type annotation
    Colon,
    Arrow,
    DoubleArrow,
    // Type declaration
    Type,
    VBar,
    // Pattern matching
    Case,
    Of,
};

pub fn formatTokenType(tt: TokenType) []const u8 {
    const name = switch (tt) {
        .CharLiteral => "a character",
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
        .BoolLiteral => "a boolean value",
        .If => "'if'",
        .Then => "'then'",
        .Else => "'else'",
        .NewStatement => "the start of a new statement",
        .Colon => "':'",
        .Arrow => "'->'",
        .DoubleArrow => "'=>'",
        .Comma => "','",
        .Type => "'type'",
        .VBar => "'|'",
        .Case => "'case'",
        .Of => "'of'",
        .LeftBracket => "'['",
        .RightBracket => "']'",
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
