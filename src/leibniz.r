rounds <- scan("rounds.txt", what = integer(), quiet = TRUE)

# Bound temporary vectors at the full benchmark size.
total <- 0
for (first in seq(-2*rounds+1+(rounds%%2)*2, 2*rounds, by = 4e6)) {
  total <- total + sum(4 / seq(first, min(first + 4*(1e6-1), 2*rounds), by = 4))
}
pi <- total

cat(sprintf("%.16f\n", pi))
