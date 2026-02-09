import Glibc

func readRounds() -> Int64 {
    var rounds: Int64 = 0
    guard let file = fopen("rounds.txt", "r") else {
        fatalError("Cannot open rounds.txt")
    }
    defer { fclose(file) }
    if fscanf(file, "%lld", &rounds) != 1 {
        fatalError("Cannot parse rounds.txt")
    }
    return rounds
}

let rounds = readRounds()
var x = 1.0
var pi = 1.0

for i in 2...(rounds + 2) {
    x *= -1.0
    pi += x / (2.0 * Double(i) - 1.0)
}

pi *= 4.0
printf("%.16f\n", pi)
