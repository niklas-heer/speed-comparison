#![feature(portable_simd)]
use std::fs::File;
use std::io::prelude::*;
use std::simd::prelude::*;

fn main() {
    let mut file = File::open("./rounds.txt").expect("file not found");

    let mut contents = String::new();
    file.read_to_string(&mut contents)
        .expect("something went wrong reading the file");

    let rounds = contents.trim().parse::<u32>().unwrap() + 2;

    // We need to use f64 instead of f32 because otherwise the precision would be off

    let x = f64x8::from_array([-1.0, 1.0, -1.0, 1.0, -1.0, 1.0, -1.0, 1.0]);
    let pi = f64x8::from_array([1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0]);

    let m2 = u32x8::splat(2);
    let s2 = u32x8::splat(1);
    let i2 = u32x8::from_array([0, 1, 2, 3, 4, 5, 6, 7]);

    let mut accum = pi;
    let mut i = 2;
    while i + 8 <= rounds {
        let ii = u32x8::splat(i);
        accum += x / (m2 * (ii + i2) - s2).cast::<f64>();
        i += 8;
    }
    let mut pi = accum.reduce_sum();
    while i < rounds {
        pi += (if i % 2 == 0 { -1.0 } else { 1.0 }) / (2 * i - 1) as f64;
        i += 1;
    }
    pi *= 4.0;

    println!("{:.16}", pi);
}
