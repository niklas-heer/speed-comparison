import java.io.File

fun main() {
    val rounds = File("rounds.txt").readText().trim().toInt()

    var pi = 1.0
    var x = 1.0

    for (i in 2 until rounds + 2) {
        x *= -1
        pi += x / (2 * i - 1)
    }

    pi *= 4
    println(pi)
}
