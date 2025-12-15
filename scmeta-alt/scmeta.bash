#!/usr/bin/env bash
# scmeta - Bash + jq variant
# Requires: jq, bc
set -euo pipefail

VERSION="1.0.0"
PI="3.141592653589793"

usage() {
    cat <<EOF
Usage: scmeta.bash [arguments]

Options:
  --lang-name=NAME        Language name
  --target-name=TARGET    Earthfile target name
  --lang-version=CMD      Command to get version
  --lang-version-match-index=N  Version match index (default 0)
  --hyperfine=FILE        Path to hyperfine JSON
  --pi=FILE               Path to pi.txt
  --output=FILE           Output JSON path
  -h, --help              Show this help
  -v, --version           Show version
EOF
}

# Calculate pi accuracy: -log10(|1 - (value / PI)|)
pi_accuracy() {
    local value="$1"
    bc -l <<EOF
scale=20
ratio = $value / $PI
diff = 1 - ratio
if (diff < 0) diff = -diff
if (diff == 0) {
    999
} else {
    -l(diff)/l(10)
}
EOF
}

# Extract version number at given index
get_version() {
    local text="$1"
    local match_index="${2:-0}"
    echo "$text" | grep -oE '[0-9]+(\.[0-9]+)+' | sed -n "$((match_index + 1))p"
}

to_timedelta() {
    echo "${1}s"
}

# Defaults
lang_name=""
target_name=""
lang_version_cmd=""
lang_version_match_index=0
hyperfine_file=""
pi_file=""
output_file=""

# Parse arguments
for arg in "$@"; do
    case "$arg" in
        -h|--help)
            usage
            exit 0
            ;;
        -v|--version)
            echo "scmeta $VERSION"
            exit 0
            ;;
        --lang-name=*)
            lang_name="${arg#*=}"
            ;;
        --target-name=*)
            target_name="${arg#*=}"
            ;;
        --lang-version=*)
            lang_version_cmd="${arg#*=}"
            ;;
        --lang-version-match-index=*)
            lang_version_match_index="${arg#*=}"
            ;;
        --hyperfine=*)
            hyperfine_file="${arg#*=}"
            ;;
        --pi=*)
            pi_file="${arg#*=}"
            ;;
        --output=*)
            output_file="${arg#*=}"
            ;;
        *)
            echo "ERROR: Unknown option: $arg" >&2
            exit 1
            ;;
    esac
done

# Validate required arguments
[[ -z "$lang_name" ]] && { echo "ERROR: --lang-name is required!" >&2; exit 1; }
[[ -z "$target_name" ]] && { echo "ERROR: --target-name is required!" >&2; exit 1; }
[[ -z "$hyperfine_file" ]] && { echo "ERROR: --hyperfine is required!" >&2; exit 1; }
[[ -z "$pi_file" ]] && { echo "ERROR: --pi is required!" >&2; exit 1; }
[[ -z "$output_file" ]] && { echo "ERROR: --output is required!" >&2; exit 1; }
[[ -z "$lang_version_cmd" ]] && { echo "ERROR: --lang-version is required!" >&2; exit 1; }

# Read pi value and calculate accuracy
computed_pi=$(tr -d '[:space:]' < "$pi_file")
[[ -z "$computed_pi" ]] && { echo "ERROR: Pi file is empty!" >&2; exit 1; }
accuracy=$(pi_accuracy "$computed_pi")

# Get language version
version_output=$(sh -c "$lang_version_cmd" 2>&1) || true
lang_version=$(get_version "$version_output" "$lang_version_match_index")
[[ -z "$lang_version" ]] && { echo "ERROR: Could not extract version from: $version_output" >&2; exit 1; }

# Read hyperfine data with jq
command_str=$(jq -r '.results[0].command' "$hyperfine_file")
mean=$(jq -r '.results[0].mean' "$hyperfine_file")
stddev=$(jq -r '.results[0].stddev' "$hyperfine_file")
user_time=$(jq -r '.results[0].user' "$hyperfine_file")
system_time=$(jq -r '.results[0].system' "$hyperfine_file")
median=$(jq -r '.results[0].median' "$hyperfine_file")
min_time=$(jq -r '.results[0].min' "$hyperfine_file")
max_time=$(jq -r '.results[0].max' "$hyperfine_file")
times=$(jq -c '.results[0].times' "$hyperfine_file")
exit_codes=$(jq -c '.results[0].exit_codes' "$hyperfine_file")

# Build output JSON
jq -n \
    --arg lang "$lang_name" \
    --arg target "$target_name" \
    --arg version "$lang_version" \
    --arg cmd "$command_str" \
    --arg pi "$computed_pi" \
    --argjson accuracy "$accuracy" \
    --arg mean "$(to_timedelta "$mean")" \
    --arg stddev "$(to_timedelta "$stddev")" \
    --arg user "$(to_timedelta "$user_time")" \
    --arg system "$(to_timedelta "$system_time")" \
    --arg median "$(to_timedelta "$median")" \
    --arg min "$(to_timedelta "$min_time")" \
    --arg max "$(to_timedelta "$max_time")" \
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
