import Foundation

let text = try! String(contentsOfFile: "rounds.txt").split(separator: "\n")[0]
let rounds = UInt64(text)! + 2
var pi = 1.0

var i: UInt64 = 2
while i < rounds {
    pi = relaxed_add(pi, -1.0 / Double(i * 2 - 1))
    i += 1

    pi = relaxed_add(pi, 1.0 / Double(i * 2 - 1))
    i += 1
}

pi *= 4.0
print(String(format: "%.16f", pi))
