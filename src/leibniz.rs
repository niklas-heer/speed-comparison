use std::fs::File;
use std::io::prelude::*;

fn main() {
    let mut file = File::open("./rounds.txt").expect("file not found");

    let mut contents = String::new();
    file.read_to_string(&mut contents)
        .expect("something went wrong reading the file");

    let rounds = &contents.trim().parse::<u32>().unwrap() + 2;

    // We need to use f64 instead of f32 because otherwise the precision would be off
    let mut x = 1.0;
    let pi = (2..rounds).fold(1.0f64, |pi, i| {
        x *= -1.0;
        pi + (x / (2u32 * i - 1) as f32) as f64
    }) * 4.0;

    println!("{:.16}", pi);
}
