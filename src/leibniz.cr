rounds = File.read("rounds.txt").to_i

x = 1.0
pi = 1.0

# There are no for loops in Crystal.
# https://github.com/crystal-lang/crystal/issues/830
2.step(to: rounds + 2) do |i|
  x *= -1
  pi += x / (2 * i - 1)
end

pi *= 4
puts "#{pi}"
