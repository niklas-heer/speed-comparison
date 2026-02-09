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

let rounds = readRounds() + 2
var pi = 1.0

var i: UInt64 = 2
while i < rounds {
    pi = relaxed_add(pi, -1.0 / Double(i * 2 - 1))
    i += 1

    pi = relaxed_add(pi, 1.0 / Double(i * 2 - 1))
    i += 1
}

pi *= 4.0
printf("%.16f\n", pi)
