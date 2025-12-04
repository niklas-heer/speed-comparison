const std = @import("std");

pub fn main() !void {
    // like C -ffast-math
    @setFloatMode(.optimized);

    var file = try std.fs.cwd().openFile("rounds.txt", .{});
    defer file.close();
    var buffer: [1024]u8 = undefined;
    const n = try file.readAll(buffer[0..buffer.len]);
    const rounds = try std.fmt.parseInt(i64, std.mem.trim(u8, buffer[0..n], "\n"), 10) + 2;

    var i: usize = 2;
    var pi: f64 = 1.0;
    while (i < rounds) : (i += 1) {
        const x: f64 = -1.0 + 2.0 * @as(f64, @floatFromInt(i & 1));
        pi += (x / @as(f64, @floatFromInt(2 * i - 1)));
    }
    pi *= 4;

    // Zig 0.15+ stdout API - use format to buffer, then write directly
    var output_buf: [64]u8 = undefined;
    const output = std.fmt.bufPrint(&output_buf, "{d:.16}", .{pi}) catch unreachable;
    const stdout = std.fs.File.stdout();
    _ = try stdout.write(output);
}
