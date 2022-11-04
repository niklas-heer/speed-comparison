using System;
using System.IO;

var data = String.Empty;

try {
    data = File.ReadAllText("rounds.txt", System.Text.Encoding.UTF8);
} catch (IOException err) {
    Console.WriteLine($"Couldn't read file:\n {err.Message}");
}

int rounds = int.Parse(data);

double pi = 1;
double x = 1;

for (int i = 2; i < rounds + 2; i++) {
    x *= -1;
    pi += (x / (2 * i - 1));
}

pi *= 4;
Console.WriteLine(pi);
