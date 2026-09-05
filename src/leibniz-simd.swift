var rounds = UInt64(readLine()!)!

var pi: Double = 1.0

let roundsRemainder = rounds % 4
rounds -= roundsRemainder

rounds += 2 // do this outside the loop

var i: UInt = 2
while i < rounds {
    let xVec = SIMD4<Double>(-1.0, 1.0, -1.0, 1.0)
    let iVec = SIMD4<Double>(Double(i) + 0, Double(i) + 1, Double(i) + 2, Double(i) + 3)
    pi += (xVec / (2.0 * iVec - 1.0)).sum()
    i += 4
}

for _ in 0..<roundsRemainder {
    let x = -1.0 + 2.0 * Double(i & 1)
    pi += x / Double(2 * i - 1)
    i += 1
}

pi *= 4.0
print(pi)
