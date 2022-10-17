import Foundation
let text = try! String(contentsOfFile: "rounds.txt")
let rounds = Int(text)!
var x = 1.0
var pi = 1.0

for i in 2...rounds+2 {
    x *= -1.0
    pi += x / (2.0 * Double(i) - 1.0)
}

pi *= 4.0
print(String(format: "%.16f", pi))
