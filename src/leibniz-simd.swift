import Glibc

func readRounds() -> UInt64 {
    var rounds: UInt64 = 0
    guard let file = fopen("rounds.txt", "r") else {
        fatalError("Cannot open rounds.txt")
    }
    defer { fclose(file) }
    if fscanf(file, "%llu", &rounds) != 1 {
        fatalError("Cannot parse rounds.txt")
    }
    return rounds
}

var rounds = readRounds()

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
printf("%.16f\n", pi)
