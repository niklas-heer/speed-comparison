use std::fs::File;
use std::io::prelude::*;

fn main() {
    let mut file = File::open("./rounds.txt").expect("file not found");

    let mut contents = String::new();
    file.read_to_string(&mut contents)
        .expect("something went wrong reading the file");

    let rounds = &contents.trim().parse::<i32>().unwrap();

    // We need to use f64 instead of f32 because otherwise the precision would be off
    let mut x: f64 = 1.0;
    let mut pi: f64 = 1.0;

    for i in 2..(rounds + 2) {
        x *= -1.0;
        pi = pi + (x / (2.0 * i as f64 - 1.0));
    }

    pi = pi * 4.0;

    println!("π = {}", pi);
}
