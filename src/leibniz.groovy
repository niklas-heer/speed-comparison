def rounds = new File("rounds.txt").text.trim().toInteger()

double x = 1.0
double pi = 1.0

for (int i = 2; i <= rounds + 2; i++) {
    x *= -1.0
    pi += x / (2 * i - 1)
}

pi *= 4.0

print pi
