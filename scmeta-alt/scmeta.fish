#!/usr/bin/env fish
# scmeta - Fish shell variant
# Requires: jq, bc

set -g VERSION "1.0.0"
set -g PI "3.141592653589793"

function usage
    echo "Usage: scmeta.fish [arguments]"
    echo ""
    echo "Options:"
    echo "  --lang-name=NAME        Language name"
    echo "  --target-name=TARGET    Earthfile target name"
    echo "  --lang-version=CMD      Command to get version"
    echo "  --lang-version-match-index=N  Version match index (default 0)"
    echo "  --hyperfine=FILE        Path to hyperfine JSON"
    echo "  --pi=FILE               Path to pi.txt"
    echo "  --output=FILE           Output JSON path"
    echo "  -h, --help              Show this help"
    echo "  -v, --version           Show version"
end

# Calculate pi accuracy: -log10(|1 - (value / PI)|)
function pi_accuracy --argument-names value
    echo "scale=20
ratio = $value / $PI
diff = 1 - ratio
if (diff < 0) diff = -diff
if (diff == 0) {
    999
} else {
    -l(diff)/l(10)
}" | bc -l
end

# Extract version number at given index
function get_version --argument-names text match_index
    set -q match_index[1]; or set match_index 0
    set -l versions (echo "$text" | grep -oE '[0-9]+(\.[0-9]+)+')
    set -l idx (math "$match_index + 1")
    if test (count $versions) -ge $idx
        echo $versions[$idx]
    end
end

function to_timedelta --argument-names num
    echo "$num"s
end

# Parse arguments using argparse
argparse 'h/help' 'v/version' 'lang-name=' 'target-name=' 'lang-version=' \
    'lang-version-match-index=' 'hyperfine=' 'pi=' 'output=' -- $argv
or exit 1

if set -q _flag_help
    usage
    exit 0
end

if set -q _flag_version
    echo "scmeta $VERSION"
    exit 0
end

# Extract values
set lang_name $_flag_lang_name
set target_name $_flag_target_name
set lang_version_cmd $_flag_lang_version
set lang_version_match_index (test -n "$_flag_lang_version_match_index"; and echo $_flag_lang_version_match_index; or echo 0)
set hyperfine_file $_flag_hyperfine
set pi_file $_flag_pi
set output_file $_flag_output

# Validate required arguments
if test -z "$lang_name"
    echo "ERROR: --lang-name is required!" >&2
    exit 1
end
if test -z "$target_name"
    echo "ERROR: --target-name is required!" >&2
    exit 1
end
if test -z "$hyperfine_file"
    echo "ERROR: --hyperfine is required!" >&2
    exit 1
end
if test -z "$pi_file"
    echo "ERROR: --pi is required!" >&2
    exit 1
end
if test -z "$output_file"
    echo "ERROR: --output is required!" >&2
    exit 1
end
if test -z "$lang_version_cmd"
    echo "ERROR: --lang-version is required!" >&2
    exit 1
end

# Read pi value and calculate accuracy
set computed_pi (string trim (cat "$pi_file"))
if test -z "$computed_pi"
    echo "ERROR: Pi file is empty!" >&2
    exit 1
end
set accuracy (pi_accuracy $computed_pi)

# Get language version
set version_output (sh -c "$lang_version_cmd" 2>&1; or true)
set lang_version (get_version "$version_output" $lang_version_match_index)
if test -z "$lang_version"
    echo "ERROR: Could not extract version from: $version_output" >&2
    exit 1
end

# Read hyperfine data with jq
set command_str (jq -r '.results[0].command' "$hyperfine_file")
set mean (jq -r '.results[0].mean' "$hyperfine_file")
set stddev (jq -r '.results[0].stddev' "$hyperfine_file")
set user_time (jq -r '.results[0].user' "$hyperfine_file")
set system_time (jq -r '.results[0].system' "$hyperfine_file")
set median (jq -r '.results[0].median' "$hyperfine_file")
set min_time (jq -r '.results[0].min' "$hyperfine_file")
set max_time (jq -r '.results[0].max' "$hyperfine_file")
set times (jq -c '.results[0].times' "$hyperfine_file")
set exit_codes (jq -c '.results[0].exit_codes' "$hyperfine_file")

# Build output JSON
jq -n \
    --arg lang "$lang_name" \
    --arg target "$target_name" \
    --arg version "$lang_version" \
    --arg cmd "$command_str" \
    --arg pi "$computed_pi" \
    --argjson accuracy "$accuracy" \
    --arg mean (to_timedelta "$mean") \
    --arg stddev (to_timedelta "$stddev") \
    --arg user (to_timedelta "$user_time") \
    --arg system (to_timedelta "$system_time") \
    --arg median (to_timedelta "$median") \
    --arg min (to_timedelta "$min_time") \
    --arg max (to_timedelta "$max_time") \
    --argjson times "$times" \
    --argjson exit_codes "$exit_codes" \
    '{
        Language: $lang,
        Target: $target,
        Version: $version,
        Command: $cmd,
        CalculatedPi: $pi,
        Accuracy: $accuracy,
        Mean: $mean,
        Stddev: $stddev,
        UserTime: $user,
        SystemTime: $system,
        Median: $median,
        Min: $min,
        Max: $max,
        TimesPerRun: $times,
        ExitCodesPerRun: $exit_codes
    }' > "$output_file"

echo "Successfully created metadata"
echo "Language: $lang_name ($lang_version)"
echo "Output: $output_file"
