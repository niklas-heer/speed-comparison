#!/usr/bin/env bash

main() {
    export TIMEFORMAT='%3lR'

    printf "\\n======= Comparison =======\\n"

    printf "Rounds used: %s\\n\\n" "$(cat ./rounds.txt)"

    printf "\\n> Python 3\\n"
    echo "Version: $(python3 --version)"
    printf "Time: "
    echo "Result: $(time python3 leibniz.py)"

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

    printf "\\n> PHP\\n"
    echo "Version: $(php7 --version)"
    printf "Time: "
    echo "Result: $(time php7 leibniz.php)"

    printf "\\n> Go\\n"
    echo "Version: $(go version)"
    go build leibniz.go
    printf "Time: "
    echo "Result: $(time ./leibniz)"
}

main
