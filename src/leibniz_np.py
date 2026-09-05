#!/usr/bin/env python
# coding=utf-8

import numpy as np


def main():
    with open("rounds.txt") as file:
        rounds = int(file.read())

    n = rounds
    # Keep vectorized summation below the runner's memory limit at one billion terms.
    total = 0.0
    start = 1 + (n % 2) * 2 - 2 * n
    for first in range(start, 2 * n + 1, 4 * 1_000_000):
        total += (1 / np.arange(first, min(first + 4 * 1_000_000, 2 * n + 1), 4)).sum()
    pi = 4 * total

    print("{:.16f}".format(pi))


main()
