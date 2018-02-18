import parseutils
import strutils

var
    rounds = parseInt($strip(readFile("rounds.txt")))
    x = 1.0
    pi = 1.0

for i in 2..(rounds + 2):
    x *= -1.0
    pi += x / float(2 * i - 1)

pi *= 4.0
echo "π = " & $pi
