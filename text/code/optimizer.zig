// Create a list of bound variables for each closure
// so not the entire env has to be cloned
pub const GatherBound = struct {
    pub fn run(ast: *AST, allocator: std.mem.Allocator) !void {
        switch (ast.*) {
            .intConstant, .floatConstant, ... => {},
            .let => |let| {
                try run(let.in, allocator);
                try run(let.be, allocator);
            },
            .ifExpr => |ifExpr| {
                try run(ifExpr.predicate, allocator);
                try run(ifExpr.thenExpr, allocator);
                try run(ifExpr.elseExpr, allocator);
            },
            ...
        }
    }
    ...
};

// Convert consecutive lambdas or calls into one object
pub const CombineCallsAndLambdas = struct {
    pub fn run(ast: *AST, allocator: std.mem.Allocator) !void {
        ...
    }
    ...
};

// Run all optimizations consecutively on a statement
pub fn optimizeStatement(
    statement: *Statement,
    allocator: std.mem.Allocator,
    interpreted: bool
) !void {
    ...
}
