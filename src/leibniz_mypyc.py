#!/usr/bin/env python
# coding=utf-8


def leibniz(rounds: int) -> float:
    pi: float = 1.0
    x: float = 1.0
    for i in range(2, rounds + 2):
        x = -x
        pi += x / (2 * i - 1)
    return pi * 4.0


def main() -> None:
    with open("rounds.txt") as file:
        rounds = int(file.read())

    pi = leibniz(rounds)
    print("{:.16f}".format(pi))


main()
