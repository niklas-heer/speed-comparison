#!/usr/bin/env raku

my $rounds = "rounds.txt".IO.slurp.trim.Int;

my num $x = 1e0;
my num $pi = 1e0;

loop (my int $i = 2; $i <= $rounds + 2; $i++) {
    $x *= -1e0;
    $pi += $x / (2 * $i - 1);
}

$pi *= 4e0;

print $pi;
