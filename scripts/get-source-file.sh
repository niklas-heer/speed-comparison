#!/bin/bash
# Get the source file(s) for a given language target from the Earthfile
# Usage: ./get-source-file.sh <language>
# Returns the source file path(s) that should be hashed for caching

set -e

LANGUAGE="$1"
EARTHFILE="${2:-Earthfile}"

if [ -z "$LANGUAGE" ]; then
    echo "Usage: $0 <language> [earthfile]" >&2
    exit 1
fi

if [ ! -f "$EARTHFILE" ]; then
    echo "Earthfile not found: $EARTHFILE" >&2
    exit 1
fi

# Extract the target block using awk
# Find content between "language:" and the next target definition
TARGET_CONTENT=$(awk "
    /^${LANGUAGE}:\$/ { found=1; next }
    found && /^[a-zA-Z][a-zA-Z0-9_-]*:\$/ { exit }
    found { print }
" "$EARTHFILE")

if [ -z "$TARGET_CONTENT" ]; then
    echo "Target '$LANGUAGE' not found in $EARTHFILE" >&2
    exit 1
fi

# Extract source files from various patterns
SOURCE_FILES=""

# Pattern 1: --src="filename" (used in +alpine and ADD_FILES)
SRC_MATCH=$(echo "$TARGET_CONTENT" | grep -oE '\-\-src="[^"]+"' | sed 's/--src="//;s/"$//' | head -1)
if [ -n "$SRC_MATCH" ]; then
    SOURCE_FILES="src/$SRC_MATCH"
fi

# Pattern 2: COPY ./src/something (direct copy)
if [ -z "$SOURCE_FILES" ]; then
    SRC_MATCH=$(echo "$TARGET_CONTENT" | grep -oE 'COPY \./src/[^ ]+' | sed 's/COPY \.\/src\///' | head -1)
    if [ -n "$SRC_MATCH" ]; then
        # Handle wildcards like cs/*.csproj -> cs directory
        if [[ "$SRC_MATCH" == *"*"* ]]; then
            SOURCE_FILES="src/$(dirname "$SRC_MATCH")"
        else
            SOURCE_FILES="src/$SRC_MATCH"
        fi
    fi
fi

# Pattern 3: Check if it references another target with source (e.g., julia-compiled references leibniz.jl)
if [ -z "$SOURCE_FILES" ]; then
    # Look for COPY ./src/leibniz patterns
    SRC_MATCH=$(echo "$TARGET_CONTENT" | grep -oE 'COPY \./leibniz[^ ]+' | sed 's/COPY \.\///' | head -1)
    if [ -n "$SRC_MATCH" ]; then
        SOURCE_FILES="src/$SRC_MATCH"
    fi
fi

if [ -z "$SOURCE_FILES" ]; then
    echo "Could not determine source file for '$LANGUAGE'" >&2
    exit 1
fi

echo "$SOURCE_FILES"
