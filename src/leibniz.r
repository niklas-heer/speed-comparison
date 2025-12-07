rounds <- scan("rounds.txt", what = integer(), quiet = TRUE)

# Process in chunks to reduce memory allocation overhead (see issue #139)
chunk_size <- 10000
n_chunks <- rounds / chunk_size

pis <- double(length = n_chunks)
from <- -2 * rounds + 1
for (kk in 1:n_chunks) {
  to <- from + 4 * (chunk_size - 1)
  pis[kk] <- sum(4 / seq.int(from, to, by = 4))
  from <- to + 4
}
pi <- sum(pis)

cat(sprintf("%.16f\n", pi))
