const std = @import("std");

pub fn main() !void {
    // like C -ffast-math
    @setFloatMode(.Optimized);

    var file = try std.fs.cwd().openFile("rounds.txt", .{});
    defer file.close();
    var buffer: [1024]u8 = undefined;
    var n = try file.readAll(buffer[0..buffer.len]);
    var rounds = try std.fmt.parseInt(i64, std.mem.trim(u8, buffer[0..n], "\n"), 10);
    rounds += 2;

    var i: usize = 2;
    var pi: f64 = 1.0;
    while (i < rounds) : (i += 1) {
        const x: f64 = -1.0 + 2.0 * @intToFloat(f64, i & 1);
        pi += (x / @intToFloat(f64, 2 * i - 1));
    }
    pi *= 4;
    try std.io.getStdOut().writer().print("{d:.16}", .{pi});
}
