<?php

$rounds = (int)file_get_contents("./rounds.txt", true);

$x = 1;
$pi = 1;

for ($i = 2; $i < $rounds + 2; $i++) {
    $x *= -1;
    $pi += ($x / (2 * $i - 1));
}

$pi *= 4;
echo("π = " . $pi);
