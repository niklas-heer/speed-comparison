rounds <- scan("rounds.txt", what = integer(), quiet = TRUE)

x <- 1.0
pi <- 1.0

for (i in 2:(rounds + 2)) {
  x <- -x
  pi <- pi + x / (2.0 * i - 1.0)
}

pi <- pi * 4.0

cat(sprintf("%.16f\n", pi))
