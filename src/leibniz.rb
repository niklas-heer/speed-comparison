rounds = Integer(File.read("./rounds.txt"))
stop = rounds + 2

x = 1.0
pi = 1.0
i = 2

while i <= stop
  x = -x
  pi += x / (2 * i - 1)
  i += 1
end

pi *= 4.0
puts pi.to_s
