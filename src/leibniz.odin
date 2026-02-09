package main

import "core:fmt"
import "core:os"

parse_int :: proc(buf: []u8) -> int{
    n := 0
    for b in buf {
        if b < '0' || b > '9' {
            break
        }
        n = n*10 + int(b - '0')
    }
    return n
}

main :: proc() {
    data, ok := os.read_entire_file("rounds.txt")
    if !ok {
        fmt.eprintln("Failed to read rounds.txt")
        return
    }

    rounds := parse_int(data)

    pi: f64 = 1.0
    x: f64 = 1.0

    for i in 2 ..= rounds + 1 {
		x = -x
		pi += x / f64(2 * i - 1)
	}

    pi *= 4.0

    buf: [64]u8
    s := fmt.bprintf(buf[:], "%.16f\n", pi)
    os.write(os.stdout, buf[:len(s)])
}
