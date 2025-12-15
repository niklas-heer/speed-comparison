#!/usr/bin/env tclsh
# scmeta - Tcl variant
# Requires: tcllib (for json package)

package require Tcl 8.6
package require json
package require json::write

set VERSION "1.0.0"
set PI 3.141592653589793

proc usage {} {
    puts "Usage: scmeta.tcl \[arguments\]"
    puts ""
    puts "Options:"
    puts "  --lang-name=NAME        Language name"
    puts "  --target-name=TARGET    Earthfile target name"
    puts "  --lang-version=CMD      Command to get version"
    puts "  --lang-version-match-index=N  Version match index (default 0)"
    puts "  --hyperfine=FILE        Path to hyperfine JSON"
    puts "  --pi=FILE               Path to pi.txt"
    puts "  --output=FILE           Output JSON path"
    puts "  -h, --help              Show this help"
    puts "  -v, --version           Show version"
}

# Calculate pi accuracy: -log10(|1 - (value / PI)|)
proc pi_accuracy {value} {
    global PI
    set ratio [expr {$value / $PI}]
    set diff [expr {abs(1 - $ratio)}]
    if {$diff == 0} {
        return 999
    }
    return [expr {-log10($diff)}]
}

# Extract version from text at given index
proc get_version {text match_index} {
    set versions {}
    set pattern {\d+\.\d+[\.\d]*}
    foreach match [regexp -all -inline $pattern $text] {
        lappend versions $match
    }
    if {[llength $versions] > $match_index} {
        return [lindex $versions $match_index]
    }
    error "No version found at index $match_index"
}

proc to_timedelta {num} {
    return "${num}s"
}

# Read file contents
proc read_file {path} {
    set f [open $path r]
    set content [read $f]
    close $f
    return $content
}

# Write to file
proc write_file {path content} {
    set f [open $path w]
    puts -nonewline $f $content
    close $f
}

# Run shell command
proc run_cmd {cmd} {
    if {[catch {exec sh -c $cmd 2>@1} result]} {
        return $result
    }
    return $result
}

# Parse command line arguments
set lang_name ""
set target_name ""
set lang_version_cmd ""
set lang_version_match_index 0
set hyperfine_file ""
set pi_file ""
set output_file ""

foreach arg $argv {
    switch -glob -- $arg {
        "-h" - "--help" {
            usage
            exit 0
        }
        "-v" - "--version" {
            puts "scmeta $VERSION"
            exit 0
        }
        "--lang-name=*" {
            set lang_name [string range $arg 12 end]
        }
        "--target-name=*" {
            set target_name [string range $arg 14 end]
        }
        "--lang-version=*" {
            set lang_version_cmd [string range $arg 15 end]
        }
        "--lang-version-match-index=*" {
            set lang_version_match_index [string range $arg 27 end]
        }
        "--hyperfine=*" {
            set hyperfine_file [string range $arg 12 end]
        }
        "--pi=*" {
            set pi_file [string range $arg 5 end]
        }
        "--output=*" {
            set output_file [string range $arg 9 end]
        }
    }
}

# Validate required arguments
if {$lang_name eq ""} { puts stderr "ERROR: --lang-name is required!"; exit 1 }
if {$target_name eq ""} { puts stderr "ERROR: --target-name is required!"; exit 1 }
if {$hyperfine_file eq ""} { puts stderr "ERROR: --hyperfine is required!"; exit 1 }
if {$pi_file eq ""} { puts stderr "ERROR: --pi is required!"; exit 1 }
if {$output_file eq ""} { puts stderr "ERROR: --output is required!"; exit 1 }
if {$lang_version_cmd eq ""} { puts stderr "ERROR: --lang-version is required!"; exit 1 }

# Read pi value and calculate accuracy
set computed_pi [string trim [read_file $pi_file]]
if {$computed_pi eq ""} {
    puts stderr "ERROR: Pi file is empty!"
    exit 1
}
set accuracy [pi_accuracy $computed_pi]

# Get language version
set version_output [run_cmd $lang_version_cmd]
set lang_version [get_version $version_output $lang_version_match_index]

# Parse hyperfine JSON
set hyperfine [json::json2dict [read_file $hyperfine_file]]
set result [lindex [dict get $hyperfine results] 0]

# Build output JSON
json::write indented 1
json::write aligned 1

set times_json [json::write array {*}[lmap t [dict get $result times] {json::write string $t}]]
set exit_codes_json [json::write array {*}[lmap e [dict get $result exit_codes] {json::write string $e}]]

set metadata_json [json::write object \
    Language [json::write string $lang_name] \
    Target [json::write string $target_name] \
    Version [json::write string $lang_version] \
    Command [json::write string [dict get $result command]] \
    CalculatedPi [json::write string $computed_pi] \
    Accuracy $accuracy \
    Mean [json::write string [to_timedelta [dict get $result mean]]] \
    Stddev [json::write string [to_timedelta [dict get $result stddev]]] \
    UserTime [json::write string [to_timedelta [dict get $result user]]] \
    SystemTime [json::write string [to_timedelta [dict get $result system]]] \
    Median [json::write string [to_timedelta [dict get $result median]]] \
    Min [json::write string [to_timedelta [dict get $result min]]] \
    Max [json::write string [to_timedelta [dict get $result max]]] \
    TimesPerRun $times_json \
    ExitCodesPerRun $exit_codes_json \
]

write_file $output_file $metadata_json

puts "Successfully created metadata"
puts "Language: $lang_name ($lang_version)"
puts "Output: $output_file"
