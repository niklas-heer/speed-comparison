local f = assert(io.open("rounds.txt", "rb"))
local content = f:read("a")
f:close()

local rounds = tonumber(content)
local stop = rounds + 2

local x = 1.0
local pi = 1.0

for i = 2, stop do
    x = -x
    pi = pi + x / (2 * i - 1)
end

pi = pi * 4.0

print(string.format("%03.16f", pi))
