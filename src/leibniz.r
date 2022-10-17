fileName <- "rounds.txt"
rounds <- as.integer(readChar(fileName, file.info(fileName)$size))
  
pi <- 4*(1+sum(c(-1,1)/((seq.int(4, 2*rounds+2, 2)-1))))

cat(sprintf("%.16f\n", pi))
