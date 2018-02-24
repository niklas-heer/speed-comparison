file = open("rounds.txt")
rounds = parse(Int64, strip(readstring(file)))

x = 1.0
pi = 1.0

for i in 2:(rounds + 2)
    x *= -1
    pi += x / (2 * i - 1)
end

pi *= 4
print(pi)
