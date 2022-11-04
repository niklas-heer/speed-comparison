import strutils

let rounds = readFile("rounds.txt").strip().parseInt()
var pi = 1.0

# -fno-signed-zeros -fno-trapping-math -fassociative-math gcc options and 32bit int loop counter allows vectorizing this loop.
# Use cuint type so that it uses the same unsigned int type used in leibniz.c.
for i in 2.cuint..(rounds.cuint + 2):
    pi += (if i mod 2 == 0: -1.0 else: 1.0) / float(2 * i - 1)

pi *= 4.0
echo pi.formatFloat(ffDecimal, 16)
