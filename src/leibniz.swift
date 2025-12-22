import Foundation
let text = try! String(contentsOfFile: "rounds.txt").split(separator: "\n")[0]
let rounds = Int(text)!

var remainingIterations = rounds + 1
var d = 3.0

// Use 4 independent accumulators to break data dependency chains,
// allowing the CPU pipeline to process calculations more efficiently.
var sum1 = 0.0
var sum2 = 0.0
var sum3 = 0.0
var sum4 = 0.0

// Unroll the loop to process 8 pairs (16 terms) per iteration
// Distributing work across accumulators keeps the CPU busy waiting for results.
while remainingIterations >= 16 {
    // Accumulator 1: pairs at d and d+4
    sum1 += -2.0 / (d * (d + 2.0)) + -2.0 / ((d + 4.0) * (d + 6.0))

    // Accumulator 2: pairs at d+8 and d+12
    sum2 += -2.0 / ((d + 8.0) * (d + 10.0)) + -2.0 / ((d + 12.0) * (d + 14.0))

    // Accumulator 3: pairs at d+16 and d+20
    sum3 += -2.0 / ((d + 16.0) * (d + 18.0)) + -2.0 / ((d + 20.0) * (d + 22.0))

    // Accumulator 4: pairs at d+24 and d+28
    sum4 += -2.0 / ((d + 24.0) * (d + 26.0)) + -2.0 / ((d + 28.0) * (d + 30.0))

    d += 32.0  // 8 pairs * 4 per pair
    remainingIterations -= 16
}

// Combine the unrolled sums
var pi = 1.0 + sum1 + sum2 + sum3 + sum4

// Cleanup: process remaining pairs sequentially
while remainingIterations >= 2 {
    pi -= 2.0 / (d * (d + 2.0))
    d += 4.0
    remainingIterations -= 2
}

// Final term if odd count
if remainingIterations > 0 {
    pi -= 1.0 / d
}

pi *= 4.0

print(String(format: "%.16f", pi))
