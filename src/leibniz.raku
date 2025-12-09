#!/usr/bin/env raku

my int $rounds = "rounds.txt".IO.slurp.trim.Int + 2;

my num $x = 1e0;
my num $pi = 1e0;
my num $w;
my num $y;

loop (my int $i = 2; $i <= $rounds; $i++) {
    $x *= -1e0;
    $y = 2 * $i - 1;
    $w = $x / $y;
    $pi += $w;
}

$pi *= 4e0;

print $pi;
