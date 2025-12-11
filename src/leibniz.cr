rounds = File.read("rounds.txt").to_i
stop = (rounds + 2).to_f64

x = 1.0_f64
pi = 1.0_f64
i = 2.0_f64

while i <= stop
  x = -x
  pi += x / (2.0_f64 * i - 1.0_f64)
  i += 1.0_f64
end

pi *= 4.0_f64
puts "#{pi}"
