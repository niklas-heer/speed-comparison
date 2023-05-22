#!/usr/bin/env python
# coding=utf-8
from typing import Iterable

def my_sum(xs: Iterable[float]) -> float:
    # we don't want to use the built in sum() because its already running mostly in a C optimized hot-path
    total = 0.0
    for x in xs:
        total += x
    return total

def leibniz(n: int) -> float:
    a = range(1 - 2*n, 2*n + 1, 4)
    b = (1.0 / float(i) for i in a)
    return 4.0 * my_sum(b)

def main() -> None:
    with open("rounds.txt") as file:
        rounds = int(file.read())

    n = rounds
    pi = leibniz(n)

    print("{:.16f}".format(pi))

main()
