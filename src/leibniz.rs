use std::fs::File;
use std::io::prelude::*;
#[allow(arithmetic_overflow)]
fn main() {
    let mut file = File::open("./rounds.txt").expect("file not found");

    let mut contents = String::new();
    file.read_to_string(&mut contents)
        .expect("something went wrong reading the file");

    let rounds = contents.trim().parse::<u32>().unwrap();

    // We need to use f64 instead of f32 because otherwise the precision would be off
    let mut pi = 0.0;
    for i in (0..rounds).step_by(8) {
        // A little loop unravelling to help with vectorization
        pi += ((2*(i&0x1)-1) as i32 as f64 / (2u32*i-1) as f64);
        let i = i+1;
        pi += ((2*(i&0x1)-1) as i32 as f64 / (2u32*i-1) as f64);
        let i = i+1;
        pi += ((2*(i&0x1)-1) as i32 as f64 / (2u32*i-1) as f64);
        let i = i+1;
        pi += ((2*(i&0x1)-1) as i32 as f64 / (2u32*i-1) as f64);
        let i = i+1;
        pi += ((2*(i&0x1)-1) as i32 as f64 / (2u32*i-1) as f64);
        let i = i+1;
        pi += ((2*(i&0x1)-1) as i32 as f64 / (2u32*i-1) as f64);
        let i = i+1;
        pi += ((2*(i&0x1)-1) as i32 as f64 / (2u32*i-1) as f64);
        let i = i+1;
        pi += ((2*(i&0x1)-1) as i32 as f64 / (2u32*i-1) as f64);
    }
    println!("{:.16}", pi*4.0);
}
