const std = @import("std");
const Token = @import("./token.zig").Token;

pub const Errors = struct {
    fileName: []const u8,
    source: []const u8,
    stderr: std.io.AnyWriter,
    allocator: std.mem.Allocator,
    errorOcurred: bool = false,

    fn printIndicator(self: *Errors, start: usize, end: usize) !void {
        const leftPad = try self.allocator.alloc(u8, start);
        defer self.allocator.free(leftPad);
        @memset(leftPad, ' ');
        const indicator = try self.allocator.alloc(u8, end - start);
        defer self.allocator.free(indicator);
        @memset(indicator, '^');
        try self.stderr.print("{s}\x1b[33m{s}\x1b[m\n", .{ leftPad, indicator });
    }

    fn printError(self: *Errors, msg: []const u8) !void {
        try self.stderr.print("\x1b[31;1merror: {s}\x1b[m", .{msg});
    }

    pub fn errorAt(self: *Errors, start: usize, end: usize, msg: []const u8) !void {
        var line: usize = 0;
        var startOfLine: usize = 0;
        var i: usize = 0;
        while (i <= start) : (i += 1) {
            if (self.source[i] == '\n') {
                line += 1;
                startOfLine = i + 1;
            }
        }
        while (i != self.source.len and self.source[i] != '\n') {
            i += 1;
        }
        try self.stderr.print("{s}:{d}:{d}: ", .{
            self.fileName,
            line + 1,
            start - startOfLine + 1,
        });
        try self.printError(msg);
        try self.stderr.print("\n{s}\n", .{self.source[startOfLine..i]});
        try self.printIndicator(start, end);
    }
};
