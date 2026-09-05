use io;
use os;
use fmt;
use strings;
use strconv;

export fn main() void = {
       const input = os::open("rounds.txt")!;
       defer io::close(input)!;
       const rounds = io::drain(input)!;
       const rounds = strings::fromutf8(rounds)!;
       const rounds = strconv::stou32(strings::trim(rounds))!;

       let pi: f64 = 1.0;

       for (let i = 2u32; i < rounds + 2u32; i += 1) {
       	   let x = -1.0f64 + (2.0 * (i & 0x1): f64);
	   pi += x / (2 * i - 1): f64;
       };

       pi *= 4.0;
       fmt::printfln("{}", pi)!;
};
