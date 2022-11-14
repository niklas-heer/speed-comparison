function f(rounds)
    pi = 1.0
    x  = -1.0
    r2 = rounds + 2
    vend = r2 - r2 % 4
    @simd for i in 2*2:4*2:(r2*2)
    #    pi += x / (2.0 * i - 1.0) -
	#	      x / (2.0 * i + 1.0) +
	#	      x / (2.0 * i + 3.0) -
	#	      x / (2.0 * i + 5.0) 
    
    # Common denominators for above:
        pi += Float64(
              -2.0f0 / fma(i, i, -1.0f0) +
              # x / (2.0 * i + 1.0) +
              -2.0f0 / (fma(i, i, 15.0f0) + 8f0i)
              # x / (2.0 * i + 5.0)
        )
    end

    for i in vend+1:r2
        pi += x / (2.0 * (i + 0.0) - 1.0)
        x = -x
    end
    return pi*4
end

rounds = parse(Int64, readchomp("rounds.txt"))
print(f(rounds))
