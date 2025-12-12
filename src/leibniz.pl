#!/usr/bin/env perl
use strict;

open(my $fh, "<rounds.txt") or die "$!";
my $rounds = <$fh> || die "$!";
my $stop = $rounds + 2.0;

my $x = 1.0;
my $pi = 1.0;
my $i = 2.0;

while ($i <= $stop) {
    $x = -$x;
    $pi += $x / (2.0 * $i - 1.0);
    $i += 1.0;
}

$pi *= 4.0;

printf("%.17g", $pi);
