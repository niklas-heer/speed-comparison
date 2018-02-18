#!/usr/bin/env bash

measure() {
    # Function which calculates how long a command runs in milliseconds
    time_start=$(date +%s%N)
    "$@"
    script_output=$?
    time_used=$((($(date +%s%N) - time_start)/1000000))
    echo "$time_used ms" >&2
    return $script_output
}

main() {
    export TIMEFORMAT='%3lR'

    printf "\\n======= Comparison =======\\n"

    printf "Rounds used: %s\\n\\n" "$(cat ./rounds.txt)"

    printf "\\n> Julia\\n"
    echo "Version: $(julia --version)"
    printf "Time: "
    echo "Result: $(measure julia leibniz.jl)"

    printf "\\n> Python 3 (CPython)\\n"
    echo "Version: $(python3 --version)"
    printf "Time: "
    echo "Result: $(measure python3 leibniz.py)"

    printf "\\n> Ruby\\n"
    echo "Version: $(ruby --version)"
    printf "Time: "
    echo "Result: $(measure ruby leibniz.rb)"

    printf "\\n> Rust\\n"
    echo "Version: $(rustc --version)"
    export RUST_BACKTRACE=1
    rustc leibniz.rs
    printf "Time: "
    echo "Result: $(measure ./leibniz)"

    printf "\\n> JS (node.js)\\n"
    echo "Version: $(node --version)"
    printf "Time: "
    echo "Result: $(measure node leibniz.js)"

    printf "\\n> Lua\\n"
    echo "Version: $(lua5.3 -v)"
    printf "Time: "
    echo "Result: $(measure lua5.3 leibniz.lua)"

    printf "\\n> PHP\\n"
    echo "Version: $(php7 --version | head -n 1)"
    printf "Time: "
    echo "Result: $(measure php7 leibniz.php)"

    printf "\\n> Crystal\\n"
    echo "Version: $(crystal --version | head -n 1)"
    crystal build leibniz.cr
    printf "Time: "
    echo "Result: $(measure ./leibniz)"

    printf "\\n> Go\\n"
    echo "Version: $(go version)"
    go build leibniz.go
    printf "Time: "
    echo "Result: $(measure ./leibniz)"

    printf "\\n> C\\n"
    echo "Version: $(gcc --version | head -n 1)"
    gcc leibniz.c -o leibniz
    printf "Time: "
    echo "Result: $(measure ./leibniz)"
}

main
