local f = assert(io.open("rounds.txt", "rb"))
local content = f:read("a")
f:close()

local rounds = tonumber(content)
local stop = rounds + 2.0

local x = 1.0
local pi = 1.0
local i = 2.0

while i <= stop do
    x = -x
    pi = pi + (x / (2.0 * i - 1.0))
    i = i + 1.0
end

pi = pi * 4.0

print(string.format("%03.16f", pi))
