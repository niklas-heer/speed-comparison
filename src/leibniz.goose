import os;
import std;

fn leibniz(rounds: i64) -> f64 {
    let double_pairs = 1 << 20;
    let block_pairs = 512;
    var remaining = rounds / 2;
    var pairs: f64 = 0.0;

    // -1/d + 1/(d+2) = -2/(d*(d+2)). Sum the smallest pairs first.
    // Beyond double_pairs the total contribution to pi is below 2^-21.
    // Widen each block sum so f32 rounding does not accumulate across blocks.
    while remaining >= double_pairs + block_pairs {
        let first = 4.0 * ((remaining - 1) as f32) + 3.0;
        var block_sum: f32 = 0.0;
        for i: i32 in 0..(block_pairs as i32) {
            let d = first - 4.0 * i;
            block_sum += 2.0 / (d * (d + 2.0));
        }
        pairs += block_sum;
        remaining -= block_pairs;
    }

    for i in 0..remaining {
        let d = 4.0 * ((remaining - 1 - i) as f64) + 3.0;
        pairs += 2.0 / (d * (d + 2.0));
    }

    let tail = if rounds % 2 != 0 { 1.0 / (2.0 * (rounds as f64) + 1.0) } else { 0.0 };
    return 4.0 * (1.0 - (pairs + tail));
}

fn main() {
    var input: u8[>..] = [];
    guard read_file("rounds.txt", input) else { abort("cannot read rounds.txt"); }
    let rounds, ok = parse_int(trim(input));
    guard ok && rounds >= 0 else { abort("rounds.txt must contain a nonnegative integer"); }

    var output: u8[>..] = [];
    format_flt(output, leibniz(rounds), 16);
    print(output);
}
