#![feature(portable_simd)]
use std::fs::File;
use std::io::prelude::*;
use std::simd::{SimdFloat, f64x8};

fn main() {
    let mut file = File::open("./rounds.txt").expect("file not found");

    let mut contents = String::new();
    file.read_to_string(&mut contents)
        .expect("something went wrong reading the file");

    let rounds = contents.trim().parse::<u32>().unwrap() + 2;

    // We need to use f64 instead of f32 because otherwise the precision would be off

    let x = f64x8::from_array([-1.0, 1.0, -1.0, 1.0, -1.0, 1.0, -1.0, 1.0]);
    let pi = f64x8::from_array([1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0]);

    let pi = (2..rounds).step_by(8).fold(pi, |pi, i| {
         pi + x / f64x8::from_array([
            f64::from(2u32 * i - 1),
            f64::from(2u32 * (i + 1) - 1),
            f64::from(2u32 * (i + 2) - 1),
            f64::from(2u32 * (i + 3) - 1),
            f64::from(2u32 * (i + 4) - 1),
            f64::from(2u32 * (i + 5) - 1),
            f64::from(2u32 * (i + 6) - 1),
            f64::from(2u32 * (i + 7) - 1),
        ])
    }).reduce_sum() * 4.0;

    println!("{:.16}", pi);
}
