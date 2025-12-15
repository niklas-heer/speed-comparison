#!/usr/bin/env qjs
// scmeta - QuickJS variant
// QuickJS has built-in JSON support and std/os modules

import * as std from 'std';
import * as os from 'os';

const VERSION = "1.0.0";
const PI = 3.141592653589793;

function usage() {
    print("Usage: scmeta.js [arguments]");
    print("");
    print("Options:");
    print("  --lang-name=NAME        Language name");
    print("  --target-name=TARGET    Earthfile target name");
    print("  --lang-version=CMD      Command to get version");
    print("  --lang-version-match-index=N  Version match index (default 0)");
    print("  --hyperfine=FILE        Path to hyperfine JSON");
    print("  --pi=FILE               Path to pi.txt");
    print("  --output=FILE           Output JSON path");
    print("  -h, --help              Show this help");
    print("  -v, --version           Show version");
}

// Calculate pi accuracy: -log10(|1 - (value / PI)|)
function piAccuracy(value) {
    const ratio = value / PI;
    const diff = Math.abs(1 - ratio);
    if (diff === 0) {
        return 999;
    }
    return -Math.log10(diff);
}

// Extract version from text at given index
function getVersion(text, matchIndex = 0) {
    const pattern = /\d+\.\d+[\.\d]*/g;
    const versions = text.match(pattern) || [];
    if (versions.length > matchIndex) {
        return versions[matchIndex];
    }
    throw new Error(`No version found at index ${matchIndex}`);
}

function toTimedelta(num) {
    return `${num}s`;
}

// Read file contents
function readFile(path) {
    const f = std.open(path, "r");
    if (!f) {
        throw new Error(`Cannot open file: ${path}`);
    }
    const content = f.readAsString();
    f.close();
    return content;
}

// Write to file
function writeFile(path, content) {
    const f = std.open(path, "w");
    if (!f) {
        throw new Error(`Cannot write to file: ${path}`);
    }
    f.puts(content);
    f.close();
}

// Run shell command and capture output
function runCmd(cmd) {
    const [output, status] = os.exec(["sh", "-c", cmd], {
        block: true,
        usePath: true,
        stdout: os.PIPE,
        stderr: os.PIPE
    });
    // QuickJS os.exec is limited; use popen approach
    const pipe = std.popen(cmd + " 2>&1", "r");
    const result = pipe.readAsString();
    pipe.close();
    return result;
}

// Parse command line arguments
let langName = null;
let targetName = null;
let langVersionCmd = null;
let langVersionMatchIndex = 0;
let hyperfineFile = null;
let piFile = null;
let outputFile = null;

const args = scriptArgs.slice(1);  // Skip script name

for (const arg of args) {
    if (arg === "-h" || arg === "--help") {
        usage();
        std.exit(0);
    } else if (arg === "-v" || arg === "--version") {
        print(`scmeta ${VERSION}`);
        std.exit(0);
    } else if (arg.startsWith("--lang-name=")) {
        langName = arg.slice("--lang-name=".length);
    } else if (arg.startsWith("--target-name=")) {
        targetName = arg.slice("--target-name=".length);
    } else if (arg.startsWith("--lang-version=")) {
        langVersionCmd = arg.slice("--lang-version=".length);
    } else if (arg.startsWith("--lang-version-match-index=")) {
        langVersionMatchIndex = parseInt(arg.slice("--lang-version-match-index=".length));
    } else if (arg.startsWith("--hyperfine=")) {
        hyperfineFile = arg.slice("--hyperfine=".length);
    } else if (arg.startsWith("--pi=")) {
        piFile = arg.slice("--pi=".length);
    } else if (arg.startsWith("--output=")) {
        outputFile = arg.slice("--output=".length);
    }
}

// Validate required arguments
if (!langName) { std.err.puts("ERROR: --lang-name is required!\n"); std.exit(1); }
if (!targetName) { std.err.puts("ERROR: --target-name is required!\n"); std.exit(1); }
if (!hyperfineFile) { std.err.puts("ERROR: --hyperfine is required!\n"); std.exit(1); }
if (!piFile) { std.err.puts("ERROR: --pi is required!\n"); std.exit(1); }
if (!outputFile) { std.err.puts("ERROR: --output is required!\n"); std.exit(1); }
if (!langVersionCmd) { std.err.puts("ERROR: --lang-version is required!\n"); std.exit(1); }

// Read pi value and calculate accuracy
const computedPi = readFile(piFile).trim();
if (!computedPi) {
    std.err.puts("ERROR: Pi file is empty!\n");
    std.exit(1);
}
const accuracy = piAccuracy(parseFloat(computedPi));

// Get language version
const versionOutput = runCmd(langVersionCmd);
const langVersion = getVersion(versionOutput, langVersionMatchIndex);

// Parse hyperfine JSON
const hyperfine = JSON.parse(readFile(hyperfineFile));
const result = hyperfine.results[0];

// Build output
const metadata = {
    Language: langName,
    Target: targetName,
    Version: langVersion,
    Command: result.command,
    CalculatedPi: computedPi,
    Accuracy: accuracy,
    Mean: toTimedelta(result.mean),
    Stddev: toTimedelta(result.stddev),
    UserTime: toTimedelta(result.user),
    SystemTime: toTimedelta(result.system),
    Median: toTimedelta(result.median),
    Min: toTimedelta(result.min),
    Max: toTimedelta(result.max),
    TimesPerRun: result.times,
    ExitCodesPerRun: result.exit_codes
};

// Write output with pretty formatting
writeFile(outputFile, JSON.stringify(metadata, null, 2) + "\n");

print("Successfully created metadata");
print(`Language: ${langName} (${langVersion})`);
print(`Output: ${outputFile}`);
