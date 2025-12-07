const rounds = parseInt(Deno.readTextFileSync("rounds.txt").trim());

let pi = 1.0;
let x = 1.0;

for (let i = 2; i <= rounds + 1; i++) {
  x = -x;
  pi += x / (2 * i - 1);
}

pi *= 4.0;
console.log(pi.toFixed(16));
