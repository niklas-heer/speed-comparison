<?php
ini_set('precision', 17); # makes it as precise as other languages

$rounds = (int)file_get_contents("./rounds.txt", true);

$x = 1.0;
$pi = 1.0;

for ($i = 2; $i < $rounds + 2; $i++) {
    $x *= -1.0;
    $pi += ($x / (float)(2.0 * $i - 1.0));
}

$pi *= 4;
print($pi);
