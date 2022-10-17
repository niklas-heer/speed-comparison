fileName <- "rounds.txt"
rounds <- as.integer(readChar(fileName, file.info(fileName)$size))
  
pi <- 4*sum(1 / seq.int(-2*rounds+1, 2*rounds, by = 4))

cat(sprintf("%.16f\n", pi))
