package main

import "core:fmt"
import "core:os"
import "core:strconv"
import "core:strings"

main :: proc() {
	data, ok := os.read_entire_file("rounds.txt")
	if !ok {
		fmt.eprintln("Failed to read rounds.txt")
		return
	}

	rounds := strconv.atoi(strings.trim_space(string(data)))

	pi: f64 = 1.0
	x: f64 = 1.0

	for i in 2 ..= rounds + 1 {
		x = -x
		pi += x / f64(2 * i - 1)
	}

	pi *= 4.0
	fmt.printf("%.16f\n", pi)
}
