const std = @import("std");

// Vector length 8 for 512 bit wide target cpus
const Vf = @Vector(8, f64);
const Vu = @Vector(8, u64);

const signs: Vf = .{ -1.0, 1.0, -1.0, 1.0, -1.0, 1.0, -1.0, 1.0 };
const offset: Vu = .{ 3, 5, 7, 9, 11, 13, 15, 17 };
const v_2: Vu = @splat(2);

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
    const rounds = try std.fmt.parseUnsigned(u64, buffer[0..n], 10);

    var i: usize = 0;
    var V_pi: Vf = @splat(0.0);
    while (i <= (rounds - 8)) : (i += 8) {
        const div: Vu = v_2 * @as(Vu, @splat(i)) + offset;

        V_pi += signs / @as(Vf, @floatFromInt(div));
    }

    var pi: f64 = 1.0 + @reduce(.Add, V_pi);

    // remaining iterations
    for (i..rounds) |itr| {
        const x: f64 = -1.0 + 2.0 * @as(f64, @floatFromInt(itr & 1));
        pi += (x / @as(f64, @floatFromInt(2 * itr - 1)));
    }
    pi *= 4;

    var output_buf: [64]u8 = undefined;
    var writer = std.Io.File.stdout().writer(io, &output_buf);

    try writer.interface.print("{d:.16}", .{pi});
    try writer.interface.flush();
}