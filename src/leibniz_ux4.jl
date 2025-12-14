function f(rounds)
    pi = -1.0
    r2 = rounds + 2
    vend = r2 - r2 % 4
    @simd for i in 4:8:(r2*2)
        pi += inv(i -  1) -
              inv(i +  1) +
              inv(i +  3) -
              inv(i +  5)
    end
    pi = -pi
    for i in vend+1:r2
        pi += 1 / (2 * i - 1)
    end
    return pi*4
end

function (@main)(ARGS)
    rounds = parse(Int64, convert(String, readchomp("rounds.txt")))
    print(Core.stdout, f(rounds))
    0
end

