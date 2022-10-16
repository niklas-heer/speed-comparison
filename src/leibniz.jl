function f(rounds)
    x = 1.0
    pi = 1.0

    for i in 2:(rounds + 2)
        x *= -1
        pi += x / (2 * i - 1)
    end

    return pi*4
end

@static if abspath(PROGRAM_FILE) == @__FILE__
    rounds = parse(Int64, readchomp("rounds.txt"))
    print(f(rounds))
end
