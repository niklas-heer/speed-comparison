#!/usr/bin/env bash

main() {
    export TIMEFORMAT='%3lR'

    printf "\\n======= Comparison =======\\n"

    printf "Rounds used: %s\\n\\n" "$(cat ./rounds.txt)"

    printf "\\n> Python 3\\n"
    echo "Version: $(python3 --version)"
    printf "Time: "
    echo "Result: $(time python3 leibniz.py)"

    printf "\\n> Ruby\\n"
    echo "Version: $(ruby --version)"
    printf "Time: "
    echo "Result: $(time ruby leibniz.rb)"

    printf "\\n> Rust\\n"
    echo "Version: $(rustc --version)"
    export RUST_BACKTRACE=1
    rustc leibniz.rs
    printf "Time: "
    echo "Result: $(time ./leibniz)"

    printf "\\n> JS (node.js)\\n"
    echo "Version: $(node --version)"
    printf "Time: "
    echo "Result: $(time node leibniz.js)"

    printf "\\n> Lua\\n"
    echo "Version: $(lua5.3 -v)"
    printf "Time: "
    echo "Result: $(time lua5.3 leibniz.lua)"

    printf "\\n> PHP\\n"
    echo "Version: $(php7 --version | head -n 1)"
    printf "Time: "
    echo "Result: $(time php7 leibniz.php)"

    printf "\\n> Crystal\\n"
    echo "Version: $(crystal --version | head -n 1)"
    crystal build leibniz.cr
    printf "Time: "
    echo "Result: $(time ./leibniz)"

    printf "\\n> Go\\n"
    echo "Version: $(go version)"
    go build leibniz.go
    printf "Time: "
    echo "Result: $(time ./leibniz)"

    printf "\\n> C\\n"
    echo "Version: $(gcc --version | head -n 1)"
    gcc leibniz.c -o leibniz
    printf "Time: "
    echo "Result: $(time ./leibniz)"
}

main
