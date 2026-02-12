use std::arch::x86_64::*;
use std::fs::File;
use std::io::prelude::*;

fn main() {
    let mut file = File::open("./rounds.txt").expect("file not found");
    let mut contents = String::new();
    file.read_to_string(&mut contents)
        .expect("something went wrong reading the file");

    let rounds = contents.trim().parse::<u32>().unwrap() + 2;

    unsafe { run_benchmark(rounds) }
}

#[target_feature(enable = "avx512f")]
unsafe fn run_benchmark(rounds: u32) {
    let unroll: u32 = 8; // 512-bit = 8x f64

    // Sign alternation vector: [-1, 1, -1, 1, -1, 1, -1, 1]
    // Note: _mm512_set_pd takes arguments in reverse order (high to low)
    let x = _mm512_set_pd(1.0, -1.0, 1.0, -1.0, 1.0, -1.0, 1.0, -1.0);

    // Constants
    let inc = _mm512_set1_pd(32.0); // Increment by 32 (4 vectors * 8)
    let two = _mm512_set1_pd(2.0);
    let mone = _mm512_set1_pd(-1.0);

    // Initial index vectors for 4 independent accumulators (reverse order for _mm512_set_pd)
    let mut ivec0 = _mm512_set_pd(9.0, 8.0, 7.0, 6.0, 5.0, 4.0, 3.0, 2.0);
    let mut ivec1 = _mm512_set_pd(17.0, 16.0, 15.0, 14.0, 13.0, 12.0, 11.0, 10.0);
    let mut ivec2 = _mm512_set_pd(25.0, 24.0, 23.0, 22.0, 21.0, 20.0, 19.0, 18.0);
    let mut ivec3 = _mm512_set_pd(33.0, 32.0, 31.0, 30.0, 29.0, 28.0, 27.0, 26.0);

    // Use 4 accumulator vectors for instruction-level parallelism
    let mut pivec0 = _mm512_setzero_pd();
    let mut pivec1 = _mm512_setzero_pd();
    let mut pivec2 = _mm512_setzero_pd();
    let mut pivec3 = _mm512_setzero_pd();

    let vec_end = rounds - (rounds - 2) % (unroll * 4);
    let mut i = 2u32;

    // Main loop: process 32 terms per iteration (4 x 8-wide vectors)
    while i < vec_end {
        // Compute denominators: 2*i - 1 (using FMA: fmadd(two, ivec, mone) would be 2*i + (-1))
        let den0 = _mm512_fmadd_pd(two, ivec0, mone);
        let den1 = _mm512_fmadd_pd(two, ivec1, mone);
        let den2 = _mm512_fmadd_pd(two, ivec2, mone);
        let den3 = _mm512_fmadd_pd(two, ivec3, mone);

        // Accumulate: pivec += x / den
        pivec0 = _mm512_add_pd(pivec0, _mm512_div_pd(x, den0));
        pivec1 = _mm512_add_pd(pivec1, _mm512_div_pd(x, den1));
        pivec2 = _mm512_add_pd(pivec2, _mm512_div_pd(x, den2));
        pivec3 = _mm512_add_pd(pivec3, _mm512_div_pd(x, den3));

        // Increment index vectors
        ivec0 = _mm512_add_pd(ivec0, inc);
        ivec1 = _mm512_add_pd(ivec1, inc);
        ivec2 = _mm512_add_pd(ivec2, inc);
        ivec3 = _mm512_add_pd(ivec3, inc);

        i += unroll * 4;
    }

    // Combine all accumulator vectors
    let pivec_sum = _mm512_add_pd(
        _mm512_add_pd(pivec0, pivec1),
        _mm512_add_pd(pivec2, pivec3)
    );

    // Horizontal sum of the vector
    let mut pi = 1.0 + _mm512_reduce_add_pd(pivec_sum);

    // Scalar cleanup loop for remaining terms
    while i < rounds {
        let xf = -1.0f64 + (2.0 * (i & 0x1) as f64);
        pi += xf / (2 * i - 1) as f64;
        i += 1;
    }

    println!("{:.16}", pi * 4.0);
}
