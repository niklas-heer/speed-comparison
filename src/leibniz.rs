#![feature(portable_simd)]
use std::fs::File;
use std::io::prelude::*;
use std::simd::{SimdFloat, f64x8, u32x8};

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

    let pi = (2..rounds).step_by(8).fold(pi, |pi, i| {
        let ii = u32x8::splat(i);
        pi + x / (m2 * (ii + i2) - s2).cast::<f64>()
    }).reduce_sum() * 4.0;

    println!("{:.16}", pi);
}
