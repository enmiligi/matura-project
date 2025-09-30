// Create a list of bound variables for each closure
// so not the entire env has to be cloned
pub const OptimizeClosures = struct {
    pub fn run(ast: *AST, allocator: std.mem.Allocator) !void {
        ...
    }
    ...
};

// Convert consecutive lambdas or calls into one object
pub const OptimizeFullyInstantiatedCalls = struct {
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
