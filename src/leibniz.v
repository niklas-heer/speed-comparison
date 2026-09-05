import os

content := os.read_file('rounds.txt')!
rounds := content.trim_space().u64()

mut pi := 1.0
for i in 2 .. rounds + 2 {
	x := -1.0 + 2.0 * f64(i & 0x1)
	pi += x / (u64(2) * i - u64(1))
}
pi *= 4.0

println('${pi:.16f}')
