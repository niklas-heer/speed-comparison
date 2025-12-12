<?php
ini_set("precision", 17); # makes it as precise as other languages
$rounds = (int) file_get_contents("./rounds.txt", true);
$stop = (float) ($rounds + 2);

$x = 1.0;
$pi = 1.0;
$i = 2.0;

while ($i <= $stop) {
    $x = -$x;
    $pi += $x / (2.0 * $i - 1.0);
    $i += 1.0;
}

$pi *= 4.0;
print $pi;
