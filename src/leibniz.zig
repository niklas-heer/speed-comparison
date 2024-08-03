const std = @import("std");

pub fn main() anyerror!void {
    var file = try std.fs.cwd().openFile("rounds.txt", .{});
    defer file.close();
    var buffer: [1024]u8 = undefined;
    const n = try file.read(buffer[0..buffer.len]);
    const rounds = try std.fmt.parseInt(u32, std.mem.trim(u8, buffer[0..n], "\n"), 10);

    const V = @Vector(4, f64);

    const sign = V{-1.0,1.0,-1.0,1.0};

    var pi: f64 = 1.0;
    var i: usize = 0;

    while (i < rounds) : (i += 4) {
        const div = V{
            @floatFromInt(2 * i + 3),
            @floatFromInt(2 * i + 5),
            @floatFromInt(2 * i + 7),
            @floatFromInt(2 * i + 9),
        };
        pi += @reduce(.Add, sign / div);
    }
    pi *= 4;

    try std.io.getStdOut().writer().print("{}", .{pi});
}
