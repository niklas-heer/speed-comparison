function f(rounds)
    pi = -1.0f0
    # x  = -1.0
    r2 = rounds + 2
    vend = r2 - r2 % 4
    @simd for i in 4:8:(r2*2)
        pi += inv(i -  1.0f0) - 
              inv(i +  1.0f0) + 
              inv(i +  3.0f0) - 
              inv(i +  5.0f0) 
    end
    pi = -pi
    for i in vend+1:r2
        pi += 1.0 / (2.0 * (i + 0.0) - 1.0)
        # x = -x
    end
    return pi*4
end

rounds = parse(Int64, readchomp("rounds.txt"))
print(f(rounds))
