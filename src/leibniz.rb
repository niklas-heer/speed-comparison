rounds = Integer(File.read("./rounds.txt"))
stop = (rounds + 2).to_f

x = 1.0
pi = 1.0
i = 2.0

while i <= stop
  x = -x
  pi += x / (2.0 * i - 1.0)
  i += 1.0
end

pi *= 4.0
puts "#{pi}"
