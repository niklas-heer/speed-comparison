const std = @import("std");

pub fn main() !void {
    // like C -ffast-math
    @setFloatMode(.optimized);

    // Zig 0.16+ Io interface - file access requires an Io implemention
    var threaded = std.Io.Threaded.init_single_threaded;
    const io = threaded.io();

    var file = try std.Io.Dir.cwd().openFile(io, "rounds.txt", .{});
    defer file.close(io);
    var buffer: [1024]u8 = undefined;
    const n = try file.readPositionalAll(io, &buffer, 0);
    const rounds = try std.fmt.parseInt(i64, std.mem.trim(u8, buffer[0..n], "\n"), 10) + 2;

    var i: usize = 2;
    var pi: f64 = 1.0;
    while (i < rounds) : (i += 1) {
        const x: f64 = -1.0 + 2.0 * @as(f64, @floatFromInt(i & 1));
        pi += (x / @as(f64, @floatFromInt(2 * i - 1)));
    }
    pi *= 4;

    var output_buf: [64]u8 = undefined;
    var writer = std.Io.File.stdout().writer(io, &output_buf);

    try writer.interface.print("{d:.16}", .{pi});
    try writer.interface.flush();
}
