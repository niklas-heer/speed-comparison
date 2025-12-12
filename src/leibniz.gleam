import gleam/io
import gleam/int
import gleam/float
import gleam/string
import simplifile

pub fn main() {
  let assert Ok(content) = simplifile.read("rounds.txt")
  let assert Ok(rounds) = int.parse(string.trim(content))
  let stop = int.to_float(rounds + 2)

  let pi = leibniz(stop, 2.0, 1.0, 1.0)
  io.println(float.to_string(pi *. 4.0))
}

fn leibniz(stop: Float, i: Float, x: Float, pi: Float) -> Float {
  case i >. stop {
    True -> pi
    False -> {
      let new_x = float.negate(x)
      let new_pi = pi +. new_x /. { 2.0 *. i -. 1.0 }
      leibniz(stop, i +. 1.0, new_x, new_pi)
    }
  }
}
