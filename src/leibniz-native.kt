@file:OptIn(kotlinx.cinterop.ExperimentalForeignApi::class)

import kotlinx.cinterop.*
import platform.posix.*

fun main(): Unit = memScoped {
    val file = fopen("rounds.txt", "r") ?: error("Cannot open rounds.txt")
    val buffer = allocArray<ByteVar>(64)
    val text = try {
        fgets(buffer, 64, file) ?: error("Cannot read rounds.txt")
        buffer.toKString().trim()
    } finally {
        fclose(file)
    }
    val rounds = text.toInt()
    var pi = 1.0
    var sign = 1.0
    for (i in 2 until rounds + 2) {
        sign = -sign
        pi += sign / (2 * i - 1)
    }
    printf("%.16f\n", pi * 4.0)
    Unit
}
