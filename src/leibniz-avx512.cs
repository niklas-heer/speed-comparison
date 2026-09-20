using System;
using System.IO;
using System.Runtime.Intrinsics;
using System.Runtime.Intrinsics.X86;

if (!Avx512F.IsSupported)
    throw new PlatformNotSupportedException("AVX-512F is required.");

var rounds = uint.Parse(File.ReadAllText("rounds.txt"));

const uint Width = 8;
const uint Unroll = 32;

rounds += 2;

var vend = rounds - ((rounds - 2) % Unroll);

// Leibniz signs:
// -1/3 + 1/5 - 1/7 + ...
var sign = Vector512.Create(
    -1.0, 1.0, -1.0, 1.0,
    -1.0, 1.0, -1.0, 1.0);

// Denominators for the first 32 terms:
//
// 3,5,7,...,65
var d0 = Vector512.Create(
     3.0,  5.0,  7.0,  9.0,
    11.0, 13.0, 15.0, 17.0);

var d1 = Vector512.Create(
    19.0, 21.0, 23.0, 25.0,
    27.0, 29.0, 31.0, 33.0);

var d2 = Vector512.Create(
    35.0, 37.0, 39.0, 41.0,
    43.0, 45.0, 47.0, 49.0);

var d3 = Vector512.Create(
    51.0, 53.0, 55.0, 57.0,
    59.0, 61.0, 63.0, 65.0);

var step = Vector512.Create(64.0);
var two = Vector512.Create(2.0);

// Four independent accumulators.
//
// Keeping these independent allows the CPU to overlap
// reciprocal/refinement work instead of waiting on one
// accumulator dependency chain.
var sum0 = Vector512<double>.Zero;
var sum1 = Vector512<double>.Zero;
var sum2 = Vector512<double>.Zero;
var sum3 = Vector512<double>.Zero;

for (uint i = 2; i < vend; i += Unroll)
{
    // AVX-512 reciprocal approximation.
    var r0 = Avx512F.Reciprocal14(d0);
    var r1 = Avx512F.Reciprocal14(d1);
    var r2 = Avx512F.Reciprocal14(d2);
    var r3 = Avx512F.Reciprocal14(d3);

    // One Newton-Raphson refinement:
    //
    // r <- r * (2 - d*r)
    //
    // This is substantially more accurate than the raw
    // Reciprocal14 approximation while remaining much cheaper
    // than full vector division on suitable CPUs.
    r0 *= two - d0 * r0;
    r1 *= two - d1 * r1;
    r2 *= two - d2 * r2;
    r3 *= two - d3 * r3;

    sum0 += sign * r0;
    sum1 += sign * r1;
    sum2 += sign * r2;
    sum3 += sign * r3;

    d0 += step;
    d1 += step;
    d2 += step;
    d3 += step;
}

// Combine the four independent vector accumulators.
var pi =
    1.0 +
    Vector512.Sum(sum0) +
    Vector512.Sum(sum1) +
    Vector512.Sum(sum2) +
    Vector512.Sum(sum3);

// Scalar remainder.
var x = 1.0;

for (var i = vend; i < rounds; ++i)
{
    x = -x;
    pi += x / (2.0 * i - 1.0);
}

pi *= 4.0;

Console.WriteLine(pi);
