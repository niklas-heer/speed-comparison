@main
def main =
  val rounds = scala.io.Source.fromFile("rounds.txt").getLines.next.trim.toInt
  var pi = 1.0

  var i = 2
  while(i < rounds + 2) {
    val x = -1.0 + 2.0 * (i & 1)
    pi += (x / (2 * i - 1))
    i += 1
  }
  pi *= 4.0

  println(pi)
