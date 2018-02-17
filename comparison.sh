#!/usr/bin/env sh

main() {
    printf "\\n======= Comparison =======\\n"

    printf "\\n> Python 3\\n"
    echo "Version: $(python3 --version)"
    echo "Time:"
    echo "Result: $(time python3 leibniz.py)"

    printf "\\n> PHP7\\n"
    echo "Version: $(php7 --version)"
    echo "Time:"
    echo "Result: $(time php7 leibniz.php)"

    printf "\\n> Rust\\n"
    echo "Version: $(rustc --version)"
    export RUST_BACKTRACE=1
    rustc leibniz.rs
    echo "Time:"
    echo "Result: $(time ./leibniz)"
}

main
