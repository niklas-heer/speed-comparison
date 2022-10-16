let rounds = 100000000


var x = 1.0
var pi = 1.0

for i in 2...rounds+2 {
    x *= -1
    pi += x / Double(2 * i - 1)
}

pi *= 4
print(pi)

// TODO:
//  * Read rounds from file
//  * Make pi more precise (16 numbers after .)
