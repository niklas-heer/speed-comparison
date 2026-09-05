#!/usr/bin/env python
# coding=utf-8

import numba

# The @numba.jit decorator with nopython=True compiles the function into machine code.
# fastmath=True enables aggressive floating-point optimizations, including auto-vectorization (SIMD).
# cache=True caches the compiled function to disk to avoid recompilation.
@numba.jit(nopython=True, fastmath=True, cache=True)
def calculate_pi_single_threaded(num_terms):
    pi_quarter = 0.0
    sign = 1.0
    for i in range(num_terms):
        denominator = 2 * i + 1.0
        pi_quarter += sign / denominator
        sign = -sign
    return pi_quarter * 4.0


def main():
    with open("rounds.txt") as file:
        rounds = int(file.read())

    # The original script at src/leibniz.py calculates `rounds + 2` terms of the series.
    # We do the same here for a fair comparison.
    num_terms = rounds + 2
    pi = calculate_pi_single_threaded(num_terms)
    print("{:.16f}".format(pi))


main()
