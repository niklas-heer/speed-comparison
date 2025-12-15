#!/usr/bin/env elvish
# scmeta - Elvish shell variant
# Elvish has built-in JSON support and structured data!
# Requires: bc (for log calculation)

use re
use str
use math
use path

var VERSION = "1.0.0"
var PI = 3.141592653589793

fn usage {
    echo "Usage: scmeta.elv [arguments]"
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
}

# Calculate pi accuracy: -log10(|1 - (value / PI)|)
fn pi-accuracy {|value|
    var ratio = (/ (num $value) $PI)
    var diff = (- 1 $ratio)
    if (< $diff 0) {
        set diff = (- 0 $diff)
    }
    if (== $diff 0) {
        put 999
    } else {
        # Elvish has math:log but we need log10
        # log10(x) = ln(x) / ln(10)
        put (/ (math:log $diff) (math:log 10) | * -1 (one))
    }
}

# Extract version numbers matching pattern
fn get-version {|text match-index|
    var versions = [(re:find '[0-9]+(\.[0-9]+)+' $text | each {|m| put $m[text]})]
    if (> (count $versions) $match-index) {
        put $versions[$match-index]
    } else {
        fail "No version at index "$match-index
    }
}

fn to-timedelta {|num|
    put $num"s"
}

# Parse arguments
var lang-name = ""
var target-name = ""
var lang-version-cmd = ""
var lang-version-match-index = 0
var hyperfine-file = ""
var pi-file = ""
var output-file = ""

var i = 0
var args = $args[1..]  # Skip script name
while (< $i (count $args)) {
    var arg = $args[$i]
    if (or (eq $arg "-h") (eq $arg "--help")) {
        usage
        exit 0
    } elif (or (eq $arg "-v") (eq $arg "--version")) {
        echo "scmeta "$VERSION
        exit 0
    } elif (str:has-prefix $arg "--lang-name=") {
        set lang-name = (str:trim-prefix $arg "--lang-name=")
    } elif (str:has-prefix $arg "--target-name=") {
        set target-name = (str:trim-prefix $arg "--target-name=")
    } elif (str:has-prefix $arg "--lang-version=") {
        set lang-version-cmd = (str:trim-prefix $arg "--lang-version=")
    } elif (str:has-prefix $arg "--lang-version-match-index=") {
        set lang-version-match-index = (num (str:trim-prefix $arg "--lang-version-match-index="))
    } elif (str:has-prefix $arg "--hyperfine=") {
        set hyperfine-file = (str:trim-prefix $arg "--hyperfine=")
    } elif (str:has-prefix $arg "--pi=") {
        set pi-file = (str:trim-prefix $arg "--pi=")
    } elif (str:has-prefix $arg "--output=") {
        set output-file = (str:trim-prefix $arg "--output=")
    }
    set i = (+ $i 1)
}

# Validate required arguments
if (eq $lang-name "") {
    echo "ERROR: --lang-name is required!" >&2
    exit 1
}
if (eq $target-name "") {
    echo "ERROR: --target-name is required!" >&2
    exit 1
}
if (eq $hyperfine-file "") {
    echo "ERROR: --hyperfine is required!" >&2
    exit 1
}
if (eq $pi-file "") {
    echo "ERROR: --pi is required!" >&2
    exit 1
}
if (eq $output-file "") {
    echo "ERROR: --output is required!" >&2
    exit 1
}
if (eq $lang-version-cmd "") {
    echo "ERROR: --lang-version is required!" >&2
    exit 1
}

# Read pi value and calculate accuracy
var computed-pi = (str:trim (slurp < $pi-file))
if (eq $computed-pi "") {
    echo "ERROR: Pi file is empty!" >&2
    exit 1
}
var accuracy = (pi-accuracy $computed-pi)

# Get language version
var version-output = ""
try {
    set version-output = (sh -c $lang-version-cmd 2>&1 | slurp)
} catch e {
    set version-output = (sh -c $lang-version-cmd 2>&1 | slurp)
}
var lang-version = (get-version $version-output $lang-version-match-index)

# Read hyperfine JSON - Elvish has native JSON parsing!
var hyperfine = (from-json < $hyperfine-file)
var result = $hyperfine[results][0]

# Build output map
var metadata = [
    &Language= $lang-name
    &Target= $target-name
    &Version= $lang-version
    &Command= $result[command]
    &CalculatedPi= $computed-pi
    &Accuracy= $accuracy
    &Mean= (to-timedelta $result[mean])
    &Stddev= (to-timedelta $result[stddev])
    &UserTime= (to-timedelta $result[user])
    &SystemTime= (to-timedelta $result[system])
    &Median= (to-timedelta $result[median])
    &Min= (to-timedelta $result[min])
    &Max= (to-timedelta $result[max])
    &TimesPerRun= $result[times]
    &ExitCodesPerRun= $result[exit_codes]
]

# Write output JSON
put $metadata | to-json > $output-file

# Pretty print it (elvish's to-json is compact)
# For pretty output we'd need jq, but this works
echo "Successfully created metadata"
echo "Language: "$lang-name" ("$lang-version")"
echo "Output: "$output-file
