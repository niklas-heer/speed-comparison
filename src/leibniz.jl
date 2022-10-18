f(n) = sum(i -> 4/(4i-2n-3), 1:n)
abspath(PROGRAM_FILE) == @__FILE__() && print(f(parse(Int, readchomp("rounds.txt"))))
