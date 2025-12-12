<?php
ini_set("precision", 17);
$rounds = (int) file_get_contents("./rounds.txt", true);
$stop = $rounds + 2;

$x = 1.0;
$pi = 1.0;

for ($i = 2; $i <= $stop; $i++) {
    $x = -$x;
    $pi += $x / (2 * $i - 1);
}

$pi *= 4.0;
print $pi;
