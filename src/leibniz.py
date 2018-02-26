#!/usr/bin/env python
# coding=utf-8
import os


with open("rounds.txt") as file:
    rounds = int(file.read())

x = 1.0
pi = 1.0

for i in range(2, rounds + 2):
    x *= -1
    pi += x / (2 * i - 1)

pi *= 4

print("{:.16f}".format(pi))
