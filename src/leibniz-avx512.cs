using System;
using System.IO;
using System.Runtime.CompilerServices;
using System.Runtime.Intrinsics;
using System.Runtime.Intrinsics.X86;

if (!Avx512F.IsSupported)
    throw new PlatformNotSupportedException("AVX-512F is required.");

var rounds = uint.Parse(File.ReadAllText("rounds.txt"));

const uint Width = 8;
const uint Unroll = 4 * Width;

// The series starts at:
//
// 1 - 1/3 + 1/5 - 1/7 + ...
//
// We process 32 terms per outer iteration.

rounds += 2;

var vend = rounds - ((rounds - 2) % Unroll);

var sign = Vector512.Create(
    -1.0, 1.0, -1.0, 1.0,
    -1.0, 1.0, -1.0, 1.0);

// Denominators:
// 3,5,7,9,11,13,15,17
var d0 = Vector512.Create(
     3.0,  5.0,  7.0,  9.0,
    11.0, 13.0, 15.0, 17.0);

// 19..33
var d1 = d0 + Vector512.Create(16.0);

// 35..49
var d2 = d1 + Vector512.Create(16.0);

// 51..65
var d3 = d2 + Vector512.Create(16.0);

// Every accumulator is independent. This removes the long
// dependency chain of a single pivec.
var sum0 = Vector512<double>.Zero;
var sum1 = Vector512<double>.Zero;
var sum2 = Vector512<double>.Zero;
var sum3 = Vector512<double>.Zero;

// Each vector contains 8 denominators.
// 32 terms * 2 = 64 denominator increment.
var denominatorStep = Vector512.Create(64.0);
var two = Vector512.Create(2.0);

for (uint i = 2; i < vend; i += Unroll)
{
    // VRCP14PD: fast reciprocal approximation.
    var r0 = Avx512F.Reciprocal14(d0);
    var r1 = Avx512F.Reciprocal14(d1);
    var r2 = Avx512F.Reciprocal14(d2);
    var r3 = Avx512F.Reciprocal14(d3);

    // One Newton-Raphson iteration:
    //
    // r = r * (2 - d*r)
    //
    // This brings the reciprocal close to full double precision.
    r0 *= two - d0 * r0;
    r1 *= two - d1 * r1;
    r2 *= two - d2 * r2;
    r3 *= two - d3 * r3;

    sum0 += sign * r0;
    sum1 += sign * r1;
    sum2 += sign * r2;
    sum3 += sign * r3;

    d0 += denominatorStep;
    d1 += denominatorStep;
    d2 += denominatorStep;
    d3 += denominatorStep;
}

var pi =
    1.0 +
    Vector512.Sum(sum0) +
    Vector512.Sum(sum1) +
    Vector512.Sum(sum2) +
    Vector512.Sum(sum3);

// Handle the tail without assuming that rounds is a multiple
// of the SIMD unroll factor.
var x = 1.0;

for (var i = vend; i < rounds; ++i)
{
    x = -x;
    pi += x / (2.0 * i - 1.0);
}

pi *= 4.0;

Console.WriteLine(pi);
