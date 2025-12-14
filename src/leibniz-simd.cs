using System.Runtime.Intrinsics;

var rounds = uint.Parse(File.ReadAllText("rounds.txt"));
var unroll = (uint)Vector512<double>.Count;

var den   = Vector512<double>.Zero;
var inc   = Vector512.Create((double)unroll);
var two   = Vector512.Create(2.0);
var mone  = Vector512.Create(-1.0);
var xvec  = Vector512.Create(-1.0, 1.0, -1.0, 1.0, -1.0, 1.0, -1.0, 1.0);
var ivec  = Vector512.Create(2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0);
var pivec = Vector512<double>.Zero;

rounds += 2;
var vend = rounds - (rounds % unroll);

for (var i = 2u; i < vend; i += unroll) {
    den   = (two * ivec) + mone;
    ivec  += inc;
    pivec += xvec / den;
}

var x  = 1.0D;
var pi = 1.0D + Vector512.Sum(pivec);

for (var i = vend; i < rounds; ++i) {
    x = -x;
    pi += x / (2 * i - 1);
}

pi *= 4;
Console.WriteLine(pi);
