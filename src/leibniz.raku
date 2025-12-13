#!/usr/bin/env raku

my int $rounds = "rounds.txt".IO.slurp.trim.Int + 2;

my num $x = 1e0;
my num $pi = 1e0;

loop (my int $i = 2; $i <= $rounds; $i++) {
    $x = -$x;
    $pi = $pi + $x / (2 * $i - 1).Num;
}

$pi = $pi * 4e0;

print $pi;
