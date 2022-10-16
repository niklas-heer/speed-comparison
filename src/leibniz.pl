#!/usr/bin/env perl
use strict;

open(my $fh, "<rounds.txt") or die "$!";
my $rounds = <$fh> || die "$!";

my $x = 1.0;
my $pi = 1.0;

for my $i (2..$rounds+2) {
    $x *= -1.0;
    $pi += $x / (2 * $i - 1);
}

$pi *= 4;

printf("%.17g", $pi);
