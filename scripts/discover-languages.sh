#!/bin/bash
# Discovers all language benchmark targets from the Earthfile
# Outputs a JSON array suitable for GitHub Actions matrix

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
EARTHFILE="${SCRIPT_DIR}/../Earthfile"

# Exclude non-language targets
EXCLUDE_TARGETS="build|alpine|analysis|all|collect-data|fast-check|test-scmeta"

# Find all lowercase targets that call DO +BENCH (language benchmarks)
languages=$(grep -B 20 "DO +BENCH" "$EARTHFILE" | grep -E "^[a-z][a-z0-9-]*:$" | sed 's/:$//' | grep -v -E "^($EXCLUDE_TARGETS)$" | sort -u)

# Output as JSON array
echo -n '['
first=true
for lang in $languages; do
    if [ "$first" = true ]; then
        first=false
    else
        echo -n ','
    fi
    echo -n "\"$lang\""
done
echo ']'
