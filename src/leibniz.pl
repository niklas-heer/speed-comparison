#!/usr/bin/env perl
use strict;

open(my $fh, "<rounds.txt") or die "$!";
my $rounds = <$fh> || die "$!";
chomp($rounds);
$rounds += 0;  # Force numeric conversion

my $x = 1.0;
my $pi = 1.0;

for (my $i = 2; $i <= $rounds + 2; $i++) {
    $x = -$x;
    $pi += $x / (2 * $i - 1);
}

$pi *= 4.0;

printf("%.17g", $pi);
