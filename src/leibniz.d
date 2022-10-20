import std.stdio;
import std.file;

double pi = 1.0;
uint rounds;

void main() {
    auto file = File("./rounds.txt", "r");
    file.readf!"%d\n"(rounds);
    rounds += 2u;
    foreach(i; 2u .. rounds) {
        double x = -1.0 + 2.0 * (i & 0x1);
        pi += (x / (2u * i - 1u));
    }
    pi *= 4.0;
    writefln("%.16f", pi);
}
