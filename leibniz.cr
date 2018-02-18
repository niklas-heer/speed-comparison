# Currently there are no for loops in Crystal
macro for(expr)
    {{expr.args.first.args.first}}.each do |{{expr.name.id}}|
        {{expr.args.first.block.body}}
    end
end

rounds = File.read("rounds.txt").to_i

x = 1.0
pi = 1.0
for i in (2..rounds+2) do
    x *= -1
    pi += x / (2 * i - 1)
end

pi *= 4
puts "π = #{pi}"
