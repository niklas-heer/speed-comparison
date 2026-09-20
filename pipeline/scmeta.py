#!/usr/bin/env python3
"""
scmeta - Speed Comparison Metadata Generator

Processes hyperfine benchmark results and generates metadata JSON.
Compatible with MicroPython for minimal container footprint.

Usage:
    python scmeta.py --lang-name="Rust" --target-name="rust" \
        --lang-version="1.75.0" --hyperfine=hyperfine.json \
        --pi=pi.txt --output=result.json
"""

import json
import math
import sys

VERSION = "2.0.0"


def pi_accuracy(value_str):
    """Calculate accuracy of computed pi value.

    Returns the number of correct decimal places (higher is better).
    """
    try:
        value = float(value_str)
    except (ValueError, TypeError):
        raise ValueError(f"Invalid pi value: '{value_str}' is not a valid number")

    if value == 0.0:
        raise ValueError("Invalid pi value: cannot be zero")

    # math.pi is available in MicroPython
    accuracy = 1 - (value / math.pi)
    if abs(accuracy) == 0.0:
        return float("inf")

    return -math.log10(abs(accuracy))


def get_version(text, match_index=0):
    """Extract version number from text.

    Simple implementation without regex (MicroPython compatible).
    Finds patterns like "1.2.3" or "21.0.1".
    """
    versions = []
    i = 0
    while i < len(text):
        # Find start of a number
        if text[i].isdigit():
            start = i
            # Consume the version pattern: digits and dots
            while i < len(text) and (text[i].isdigit() or text[i] == "."):
                i += 1
            version = text[start:i]
            # Must contain at least one dot to be a version
            if "." in version and not version.endswith("."):
                versions.append(version)
        else:
            i += 1

    if not versions:
        raise ValueError(f"No version number found in: '{text}'")

    if match_index < 0 or match_index >= len(versions):
        available = "\n".join(f"  {i}: {v}" for i, v in enumerate(versions))
        raise ValueError(f"Invalid match index {match_index}. Available versions:\n{available}")

    return versions[match_index]


def to_timedelta(number):
    """Format number as seconds string."""
    return f"{number}s"


def parse_args(argv):
    """Parse command line arguments.

    Simple implementation without argparse (MicroPython compatible).
    """
    args = {
        "lang_name": None,
        "target_name": None,
        "lang_version": None,
        "hyperfine": None,
        "pi": None,
        "output": None,
    }

    i = 1  # Skip script name
    while i < len(argv):
        arg = argv[i]

        if arg in ("-v", "--version"):
            print(f"scmeta {VERSION}")
            sys.exit(0)

        if arg in ("-h", "--help"):
            print(__doc__)
            sys.exit(0)

        if "=" in arg:
            key, value = arg.split("=", 1)
        elif i + 1 < len(argv) and not argv[i + 1].startswith("-"):
            key = arg
            value = argv[i + 1]
            i += 1
        else:
            print(f"ERROR: {arg} requires a value", file=sys.stderr)
            sys.exit(1)

        # Map argument names to dict keys
        if key in ("--lang-name",):
            args["lang_name"] = value
        elif key in ("--target-name",):
            args["target_name"] = value
        elif key in ("--lang-version",):
            args["lang_version"] = value
        elif key in ("--hyperfine",):
            args["hyperfine"] = value
        elif key in ("--pi",):
            args["pi"] = value
        elif key in ("--output",):
            args["output"] = value
        else:
            print(f"ERROR: Unknown option {key}", file=sys.stderr)
            sys.exit(1)

        i += 1

    return args


def main():
    args = parse_args(sys.argv)

    # Validate required arguments
    required = ["lang_name", "target_name", "lang_version", "hyperfine", "pi", "output"]
    for name in required:
        if args[name] is None:
            opt_name = "--" + name.replace("_", "-")
            print(f"ERROR: {opt_name} is required!", file=sys.stderr)
            sys.exit(1)

    # Read hyperfine JSON
    try:
        with open(args["hyperfine"], "r") as f:
            hyperfine = json.load(f)
    except Exception as e:
        print(f"ERROR: Failed to read hyperfine file: {e}", file=sys.stderr)
        sys.exit(1)

    # Read computed pi value
    try:
        with open(args["pi"], "r") as f:
            computed_pi = f.read().strip()
    except Exception as e:
        print(f"ERROR: Failed to read pi file: {e}", file=sys.stderr)
        sys.exit(1)

    if not computed_pi:
        print("ERROR: Pi file is empty!", file=sys.stderr)
        sys.exit(1)

    # Calculate accuracy
    try:
        accuracy = pi_accuracy(computed_pi)
    except ValueError as e:
        print(f"ERROR: {e}", file=sys.stderr)
        sys.exit(1)

    # Extract version from provided version string
    try:
        version = get_version(args["lang_version"])
    except ValueError as e:
        # If no version pattern found, use the raw string
        version = args["lang_version"].strip()

    # Get hyperfine results
    results = hyperfine.get("results", [{}])[0]

    # Build metadata
    metadata = {
        "Language": args["lang_name"],
        "Target": args["target_name"],
        "Version": version,
        "Command": results.get("command", ""),
        "CalculatedPi": computed_pi,
        "Accuracy": accuracy,
        "Mean": to_timedelta(results.get("mean", 0)),
        "Stddev": to_timedelta(results.get("stddev", 0)),
        "UserTime": to_timedelta(results.get("user", 0)),
        "SystemTime": to_timedelta(results.get("system", 0)),
        "Median": to_timedelta(results.get("median", 0)),
        "Min": to_timedelta(results.get("min", 0)),
        "Max": to_timedelta(results.get("max", 0)),
        "TimesPerRun": results.get("times", []),
        "ExitCodesPerRun": results.get("exit_codes", []),
    }

    # Write output
    try:
        with open(args["output"], "w") as f:
            # MicroPython's json.dump doesn't have indent, so we format manually
            json_str = json.dumps(metadata)
            # Pretty print if possible (CPython), otherwise just dump
            try:
                f.write(json.dumps(metadata, indent=2))
            except TypeError:
                f.write(json_str)
    except Exception as e:
        print(f"ERROR: Failed to write output file: {e}", file=sys.stderr)
        sys.exit(1)

    print("Successfully created metadata")
    print(f"Language: {args['lang_name']} ({version})")
    print(f"Output: {args['output']}")


if __name__ == "__main__":
    main()
