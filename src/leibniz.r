rounds <- scan("rounds.txt", what = integer(), quiet = TRUE)

pi <- sum(4 / seq.int(-2*rounds+1+(rounds%%2)*2, 2*rounds, by = 4))

cat(sprintf("%.16f\n", pi))
