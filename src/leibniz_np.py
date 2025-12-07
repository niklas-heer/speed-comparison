#!/usr/bin/env python
# coding=utf-8

import numpy as np


def main():
    with open("rounds.txt") as file:
        rounds = int(file.read())

    n = rounds
    pi = 4 * (1 / np.arange(1 - 2 * n, 2 * n + 1, 4)).sum()

    print("{:.16f}".format(pi))


main()
