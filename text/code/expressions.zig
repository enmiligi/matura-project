pub fn expression(self: *Parser, rbp: Precedence) !*AST {
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
