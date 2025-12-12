#!/usr/bin/env python
# coding=utf-8

# https://stackoverflow.com/questions/11241523/why-does-python-code-run-faster-in-a-function
def main():
    with open("rounds.txt") as file:
        rounds = int(file.read())

    stop = float(rounds + 2)
    x = 1.0
    pi = 1.0
    i = 2.0

    while i <= stop:
        x = -x
        pi += x / (2.0 * i - 1.0)
        i += 1.0

    pi *= 4.0
    print("{:.16f}".format(pi))


main()
