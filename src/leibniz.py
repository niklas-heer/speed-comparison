#!/usr/bin/env python
# coding=utf-8

# https://stackoverflow.com/questions/11241523/why-does-python-code-run-faster-in-a-function
def main():
    with open("rounds.txt") as file:
        rounds = int(file.read())

    n = rounds
    pi = 4 * sum(1 / i for i in range(1 - 2*n, 2*n + 1, 4))

    print("{:.16f}".format(pi))

main()
