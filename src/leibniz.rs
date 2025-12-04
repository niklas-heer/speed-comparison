use std::fs::File;
use std::io::prelude::*;

fn main() {
    let mut file = File::open("./rounds.txt").expect("file not found");

    let mut contents = String::new();
    file.read_to_string(&mut contents)
        .expect("something went wrong reading the file");

    let rounds = contents.trim().parse::<u32>().unwrap() + 2;

    let mut pi: f64 = 1.0;
    (2..rounds).foreach(|i| {
        let x = -1.0f64 + (2.0 * (i & 0x1) as f64);
        pi += x / (2 * i - 1) as f64;
    });

    println!("{:.16}", pi * 4.0);
}
