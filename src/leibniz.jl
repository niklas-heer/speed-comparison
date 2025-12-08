function f(rounds)
    pi = 1.0
    @fastmath @simd for i in 2:(rounds + 2)
        pi += (iseven(i) ? -1 : 1) / (2*i - 1)
    end
    return pi*4
end

function (@main)(ARGS)
    rounds = parse(Int64, convert(String, readchomp("rounds.txt")))
    print(Core.stdout, f(rounds))
    0
end
