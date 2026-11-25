package main

import "core:fmt"
import "core:os"
import "core:simd"
import "core:strings"

leibniz :: proc(rounds: i64) -> f64 {
	DOUBLE_PAIRS :: 1 << 20
	BLOCK_PAIRS :: 512
	LANES32 :: 16
	LANES64 :: 8
	Vec32 :: #simd[LANES32]f32
	Vec64 :: #simd[LANES64]f64

	remaining := rounds / 2
	pairs: f64

	// -1/d + 1/(d+2) = -2/(d*(d+2)). Sum the smallest pairs first.
	// Beyond DOUBLE_PAIRS the total contribution to pi is below 2^-21.
	// Widen each block sum so f32 rounding does not accumulate across blocks.
	for ; remaining >= DOUBLE_PAIRS + BLOCK_PAIRS; remaining -= BLOCK_PAIRS {
		first := 4.0 * f32(remaining - 1) + 3.0
		offsets := Vec32{0, 4, 8, 12, 16, 20, 24, 28, 32, 36, 40, 44, 48, 52, 56, 60}
		block_sum: Vec32
		for i := 0; i < BLOCK_PAIRS; i += LANES32 {
			d := first - offsets
			block_sum += 2.0 / (d * (d + 2.0))
			offsets += 4 * LANES32
		}
		pairs += f64(simd.reduce_add_bisect(block_sum))
	}

	den := 4.0 * f64(remaining) + Vec64{-1, -5, -9, -13, -17, -21, -25, -29}
	lanes: Vec64
	for ; remaining >= LANES64; remaining -= LANES64 {
		lanes += 2.0 / (den * (den + 2.0))
		den -= 4 * LANES64
	}
	pairs += simd.reduce_add_bisect(lanes)

	for ; remaining > 0; remaining -= 1 {
		d := 4.0 * f64(remaining - 1) + 3.0
		pairs += 2.0 / (d * (d + 2.0))
	}

	if rounds % 2 != 0 {
		pairs += 1.0 / (2.0 * f64(rounds) + 1.0)
	}
	return 4.0 * (1.0 - pairs)
}

parse_rounds :: proc(s: string) -> (i64, bool) {
	rounds: i64
	for c in s {
		digit := i64(c - '0')
		if digit < 0 || digit > 9 || rounds > (max(i64) - digit) / 10 {
			return 0, false
		}
		rounds = rounds * 10 + digit
	}
	return rounds, len(s) != 0
}

main :: proc() {
	input, err := os.read_entire_file("rounds.txt", context.allocator)
	if err != nil {
		fmt.eprintln("cannot read rounds.txt")
		os.exit(1)
	}
	defer delete(input)

	rounds, ok := parse_rounds(strings.trim_space(string(input)))
	if !ok {
		fmt.eprintln("rounds.txt must contain a nonnegative integer")
		os.exit(1)
	}
	fmt.printf("%.16f\n", leibniz(rounds))
}
