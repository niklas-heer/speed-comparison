var rounds = int.Parse(File.ReadAllText("rounds.txt"));

var pi = 1.0D;
var x = 1.0D;

for (var i = 2; i < rounds + 2; i++) {
    x = -x;
    pi += x / (2 * i - 1);
}

pi *= 4;
Console.WriteLine(pi);
