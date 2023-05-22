function f(rounds)
    pi = 1.0
    x  = -1.0
    r2 = rounds + 2
    vend = Int64(r2 - r2) % 8
    @simd for i in 2*2:8*2:(r2*2)
    # Common-denominators method, half as many divisions:
        pi += Float64(
               -2.0f0 / fma(i, i, -1.0f0) +
               # x / (2.0 * i + 1.0) +
               -2.0f0 / (fma(i, i, 15.0f0) + 8f0i) +
               # x / (2.0 * i + 5.0) +
               -2.0f0 / (fma(i, i, 63f0) + 16f0i) +
               # x / (2.0 * i + 9.0) +
               -2.0f0 / (fma(i, i, 143f0) + 24f0i)
               # x / (2.0 * i + 13.0)
        )
    end

    for i in vend+1:r2
        pi += x / (2.0f0 * i - 1.0f0)
        x = -x
    end

    return pi*4
end

@static if abspath(PROGRAM_FILE) == @__FILE__
    rounds = parse(Int64, readchomp("rounds.txt"))
    print(f(rounds))
end
