const std = @import("std");
const json = std.json;
const fs = std.fs;
const mem = std.mem;
const math = std.math;
const process = std.process;

const VERSION = "1.0.0";
const PI: f64 = 3.141592653589793;

const Args = struct {
    lang_name: ?[]const u8 = null,
    target_name: ?[]const u8 = null,
    lang_version_cmd: ?[]const u8 = null,
    lang_version_match_index: usize = 0,
    hyperfine_file: ?[]const u8 = null,
    pi_file: ?[]const u8 = null,
    output_file: ?[]const u8 = null,
};

// Hyperfine JSON structure
const HyperfineResult = struct {
    command: []const u8,
    mean: f64,
    stddev: f64,
    user: f64,
    system: f64,
    median: f64,
    min: f64,
    max: f64,
    times: []f64,
    exit_codes: []i32,
};

const Hyperfine = struct {
    results: []HyperfineResult,
};

fn usage() void {
    const help =
        \\Usage: scmeta [arguments]
        \\
        \\Options:
        \\  --lang-name=NAME        Language name
        \\  --target-name=TARGET    Earthfile target name
        \\  --lang-version=CMD      Command to get version
        \\  --lang-version-match-index=N  Version match index (default 0)
        \\  --hyperfine=FILE        Path to hyperfine JSON
        \\  --pi=FILE               Path to pi.txt
        \\  --output=FILE           Output JSON path
        \\  -h, --help              Show this help
        \\  -v, --version           Show version
        \\
    ;
    std.debug.print("{s}", .{help});
}

/// Calculate pi accuracy: -log10(|1 - (value / PI)|)
fn piAccuracy(value: f64) f64 {
    const ratio = value / PI;
    const diff = @abs(1.0 - ratio);
    if (diff == 0.0) {
        return 999.0;
    }
    return -math.log10(diff);
}

/// Extract version from text at given index
fn getVersion(allocator: std.mem.Allocator, text: []const u8, match_index: usize) ![]const u8 {
    var versions = std.ArrayList([]const u8).init(allocator);
    defer versions.deinit();

    var i: usize = 0;
    while (i < text.len) {
        // Find start of a number
        if (std.ascii.isDigit(text[i])) {
            const start = i;
            var has_dot = false;
            while (i < text.len and (std.ascii.isDigit(text[i]) or text[i] == '.')) {
                if (text[i] == '.') has_dot = true;
                i += 1;
            }
            if (has_dot) {
                // Trim trailing dots
                var end = i;
                while (end > start and text[end - 1] == '.') {
                    end -= 1;
                }
                if (mem.indexOf(u8, text[start..end], ".") != null) {
                    try versions.append(text[start..end]);
                }
            }
        } else {
            i += 1;
        }
    }

    if (versions.items.len > match_index) {
        return versions.items[match_index];
    }
    return error.NoVersionFound;
}

/// Run shell command and capture output
fn runCmd(allocator: std.mem.Allocator, cmd: []const u8) ![]const u8 {
    var child = std.process.Child.init(.{
        .argv = &[_][]const u8{ "sh", "-c", cmd },
        .allocator = allocator,
    }, allocator);
    child.stdout_behavior = .Pipe;
    child.stderr_behavior = .Pipe;

    try child.spawn();

    const stdout = try child.stdout.?.reader().readAllAlloc(allocator, 1024 * 1024);
    const stderr = try child.stderr.?.reader().readAllAlloc(allocator, 1024 * 1024);
    defer allocator.free(stderr);

    _ = try child.wait();

    if (stdout.len > 0) return stdout;
    return stderr;
}

/// Read entire file
fn readFile(allocator: std.mem.Allocator, path: []const u8) ![]const u8 {
    const file = try fs.cwd().openFile(path, .{});
    defer file.close();
    return try file.readToEndAlloc(allocator, 1024 * 1024);
}

/// Write to file
fn writeFile(path: []const u8, content: []const u8) !void {
    const file = try fs.cwd().createFile(path, .{});
    defer file.close();
    try file.writeAll(content);
}

/// Parse command line arguments
fn parseArgs(args: []const [:0]const u8) Args {
    var result = Args{};

    for (args) |arg| {
        if (mem.eql(u8, arg, "-h") or mem.eql(u8, arg, "--help")) {
            usage();
            process.exit(0);
        } else if (mem.eql(u8, arg, "-v") or mem.eql(u8, arg, "--version")) {
            std.debug.print("scmeta {s}\n", .{VERSION});
            process.exit(0);
        } else if (mem.startsWith(u8, arg, "--lang-name=")) {
            result.lang_name = arg["--lang-name=".len..];
        } else if (mem.startsWith(u8, arg, "--target-name=")) {
            result.target_name = arg["--target-name=".len..];
        } else if (mem.startsWith(u8, arg, "--lang-version=")) {
            result.lang_version_cmd = arg["--lang-version=".len..];
        } else if (mem.startsWith(u8, arg, "--lang-version-match-index=")) {
            const num_str = arg["--lang-version-match-index=".len..];
            result.lang_version_match_index = std.fmt.parseInt(usize, num_str, 10) catch 0;
        } else if (mem.startsWith(u8, arg, "--hyperfine=")) {
            result.hyperfine_file = arg["--hyperfine=".len..];
        } else if (mem.startsWith(u8, arg, "--pi=")) {
            result.pi_file = arg["--pi=".len..];
        } else if (mem.startsWith(u8, arg, "--output=")) {
            result.output_file = arg["--output=".len..];
        }
    }

    return result;
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const args = try process.argsAlloc(allocator);
    defer process.argsFree(allocator, args);

    const parsed_args = parseArgs(args[1..]);

    // Validate required arguments
    const lang_name = parsed_args.lang_name orelse {
        std.debug.print("ERROR: --lang-name is required!\n", .{});
        process.exit(1);
    };
    const target_name = parsed_args.target_name orelse {
        std.debug.print("ERROR: --target-name is required!\n", .{});
        process.exit(1);
    };
    const hyperfine_file = parsed_args.hyperfine_file orelse {
        std.debug.print("ERROR: --hyperfine is required!\n", .{});
        process.exit(1);
    };
    const pi_file = parsed_args.pi_file orelse {
        std.debug.print("ERROR: --pi is required!\n", .{});
        process.exit(1);
    };
    const output_file = parsed_args.output_file orelse {
        std.debug.print("ERROR: --output is required!\n", .{});
        process.exit(1);
    };
    const lang_version_cmd = parsed_args.lang_version_cmd orelse {
        std.debug.print("ERROR: --lang-version is required!\n", .{});
        process.exit(1);
    };

    // Read pi value and calculate accuracy
    const pi_content = try readFile(allocator, pi_file);
    defer allocator.free(pi_content);
    const computed_pi = mem.trim(u8, pi_content, &std.ascii.whitespace);
    if (computed_pi.len == 0) {
        std.debug.print("ERROR: Pi file is empty!\n", .{});
        process.exit(1);
    }
    const pi_value = try std.fmt.parseFloat(f64, computed_pi);
    const accuracy = piAccuracy(pi_value);

    // Get language version
    const version_output = try runCmd(allocator, lang_version_cmd);
    defer allocator.free(version_output);
    const lang_version = try getVersion(allocator, version_output, parsed_args.lang_version_match_index);

    // Parse hyperfine JSON
    const hyperfine_content = try readFile(allocator, hyperfine_file);
    defer allocator.free(hyperfine_content);
    const hyperfine = try json.parseFromSlice(Hyperfine, allocator, hyperfine_content, .{});
    defer hyperfine.deinit();
    const result = hyperfine.value.results[0];

    // Build output JSON
    var output = std.ArrayList(u8).init(allocator);
    defer output.deinit();
    const writer = output.writer();

    try writer.print(
        \\{{
        \\  "Language": "{s}",
        \\  "Target": "{s}",
        \\  "Version": "{s}",
        \\  "Command": "{s}",
        \\  "CalculatedPi": "{s}",
        \\  "Accuracy": {d},
        \\  "Mean": "{d}s",
        \\  "Stddev": "{d}s",
        \\  "UserTime": "{d}s",
        \\  "SystemTime": "{d}s",
        \\  "Median": "{d}s",
        \\  "Min": "{d}s",
        \\  "Max": "{d}s",
        \\  "TimesPerRun": [
    , .{
        lang_name,
        target_name,
        lang_version,
        result.command,
        computed_pi,
        accuracy,
        result.mean,
        result.stddev,
        result.user,
        result.system,
        result.median,
        result.min,
        result.max,
    });

    // Write times array
    for (result.times, 0..) |t, i| {
        if (i > 0) try writer.writeAll(", ");
        try writer.print("{d}", .{t});
    }
    try writer.writeAll("],\n  \"ExitCodesPerRun\": [");

    // Write exit codes array
    for (result.exit_codes, 0..) |e, i| {
        if (i > 0) try writer.writeAll(", ");
        try writer.print("{d}", .{e});
    }
    try writer.writeAll("]\n}\n");

    // Write output file
    try writeFile(output_file, output.items);

    std.debug.print("Successfully created metadata\n", .{});
    std.debug.print("Language: {s} ({s})\n", .{ lang_name, lang_version });
    std.debug.print("Output: {s}\n", .{output_file});
}
