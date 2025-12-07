import gleam/io
import gleam/int
import gleam/float
import gleam/string
import simplifile

pub fn main() {
  let assert Ok(content) = simplifile.read("rounds.txt")
  let assert Ok(rounds) = int.parse(string.trim(content))

  let pi = leibniz(rounds, 2, 1.0, 1.0)
  io.println(float.to_string(pi *. 4.0))
}

fn leibniz(rounds: Int, i: Int, x: Float, pi: Float) -> Float {
  case i > rounds + 1 {
    True -> pi
    False -> {
      let new_x = float.negate(x)
      let new_pi = pi +. new_x /. int.to_float(2 * i - 1)
      leibniz(rounds, i + 1, new_x, new_pi)
    }
  }
}
