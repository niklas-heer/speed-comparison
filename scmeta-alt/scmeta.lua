#!/usr/bin/env lua
-- scmeta - Lua variant
-- Requires: lua-cjson (apk add lua5.4-cjson)

local cjson = require("cjson")

local VERSION = "1.0.0"
local PI = 3.141592653589793

local function usage()
    print("Usage: scmeta.lua [arguments]")
    print("")
    print("Options:")
    print("  --lang-name=NAME        Language name")
    print("  --target-name=TARGET    Earthfile target name")
    print("  --lang-version=CMD      Command to get version")
    print("  --lang-version-match-index=N  Version match index (default 0)")
    print("  --hyperfine=FILE        Path to hyperfine JSON")
    print("  --pi=FILE               Path to pi.txt")
    print("  --output=FILE           Output JSON path")
    print("  -h, --help              Show this help")
    print("  -v, --version           Show version")
end

-- Calculate pi accuracy: -log10(|1 - (value / PI)|)
local function pi_accuracy(value)
    local ratio = value / PI
    local diff = math.abs(1 - ratio)
    if diff == 0 then
        return 999
    end
    return -math.log10(diff)
end

-- Extract version from text at given index
local function get_version(text, match_index)
    match_index = match_index or 0
    local versions = {}
    for ver in text:gmatch("%d+%.%d+[%.%d]*") do
        table.insert(versions, ver)
    end
    if #versions > match_index then
        return versions[match_index + 1]  -- Lua is 1-indexed
    end
    error("No version found at index " .. match_index)
end

local function to_timedelta(num)
    return tostring(num) .. "s"
end

-- Read entire file
local function read_file(path)
    local f = io.open(path, "r")
    if not f then
        error("Cannot open file: " .. path)
    end
    local content = f:read("*all")
    f:close()
    return content
end

-- Write to file
local function write_file(path, content)
    local f = io.open(path, "w")
    if not f then
        error("Cannot write to file: " .. path)
    end
    f:write(content)
    f:close()
end

-- Run shell command and capture output
local function run_cmd(cmd)
    local handle = io.popen(cmd .. " 2>&1")
    local result = handle:read("*all")
    handle:close()
    return result
end

-- Parse command line arguments
local lang_name = nil
local target_name = nil
local lang_version_cmd = nil
local lang_version_match_index = 0
local hyperfine_file = nil
local pi_file = nil
local output_file = nil

for i = 1, #arg do
    local a = arg[i]
    if a == "-h" or a == "--help" then
        usage()
        os.exit(0)
    elseif a == "-v" or a == "--version" then
        print("scmeta " .. VERSION)
        os.exit(0)
    elseif a:match("^--lang%-name=") then
        lang_name = a:gsub("^--lang%-name=", "")
    elseif a:match("^--target%-name=") then
        target_name = a:gsub("^--target%-name=", "")
    elseif a:match("^--lang%-version=") then
        lang_version_cmd = a:gsub("^--lang%-version=", "")
    elseif a:match("^--lang%-version%-match%-index=") then
        lang_version_match_index = tonumber(a:gsub("^--lang%-version%-match%-index=", ""))
    elseif a:match("^--hyperfine=") then
        hyperfine_file = a:gsub("^--hyperfine=", "")
    elseif a:match("^--pi=") then
        pi_file = a:gsub("^--pi=", "")
    elseif a:match("^--output=") then
        output_file = a:gsub("^--output=", "")
    end
end

-- Validate required arguments
if not lang_name then io.stderr:write("ERROR: --lang-name is required!\n"); os.exit(1) end
if not target_name then io.stderr:write("ERROR: --target-name is required!\n"); os.exit(1) end
if not hyperfine_file then io.stderr:write("ERROR: --hyperfine is required!\n"); os.exit(1) end
if not pi_file then io.stderr:write("ERROR: --pi is required!\n"); os.exit(1) end
if not output_file then io.stderr:write("ERROR: --output is required!\n"); os.exit(1) end
if not lang_version_cmd then io.stderr:write("ERROR: --lang-version is required!\n"); os.exit(1) end

-- Read pi value and calculate accuracy
local computed_pi = read_file(pi_file):match("^%s*(.-)%s*$")  -- trim
if computed_pi == "" then
    io.stderr:write("ERROR: Pi file is empty!\n")
    os.exit(1)
end
local accuracy = pi_accuracy(tonumber(computed_pi))

-- Get language version
local version_output = run_cmd(lang_version_cmd)
local lang_version = get_version(version_output, lang_version_match_index)

-- Parse hyperfine JSON
local hyperfine = cjson.decode(read_file(hyperfine_file))
local result = hyperfine.results[1]

-- Build output
local metadata = {
    Language = lang_name,
    Target = target_name,
    Version = lang_version,
    Command = result.command,
    CalculatedPi = computed_pi,
    Accuracy = accuracy,
    Mean = to_timedelta(result.mean),
    Stddev = to_timedelta(result.stddev),
    UserTime = to_timedelta(result.user),
    SystemTime = to_timedelta(result.system),
    Median = to_timedelta(result.median),
    Min = to_timedelta(result.min),
    Max = to_timedelta(result.max),
    TimesPerRun = result.times,
    ExitCodesPerRun = result.exit_codes
}

-- Write output with pretty formatting
-- cjson.encode produces compact JSON; for pretty we'd need a helper
local json_output = cjson.encode(metadata)
write_file(output_file, json_output .. "\n")

print("Successfully created metadata")
print("Language: " .. lang_name .. " (" .. lang_version .. ")")
print("Output: " .. output_file)
