import os

fn main() {
	content := os.read_file('rounds.txt') or { panic(err) }
	rounds := content.trim_space().int()

	mut pi := 1.0
	mut x := 1.0

	for i in 2 .. rounds + 2 {
		x = -x
		pi += x / f64(2 * i - 1)
	}

	pi *= 4.0
	println('${pi:.16f}')
}
