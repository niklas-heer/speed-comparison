function readAll(file)
    local f = assert(io.open(file, "rb"))
    local content = f:read("*all")
    f:close()
    return content
end

rounds = tonumber(readAll("rounds.txt"))

x = 1.0
pi = 1.0

for i = 2, rounds + 2 do
    x = x * -1
    pi = pi + (x / (2 * i - 1))
end

pi = pi * 4

print("π = " .. string.format("%03.16f\n",pi))
