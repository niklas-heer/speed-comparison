struct SignVector <: AbstractVector{Float64}
    len::Int
end
Base.size(s::SignVector) = (s.len,)
Base.getindex(::SignVector, i::Int) = Float64((-1)^iseven(i))

function f(rounds)
    xs = SignVector(rounds + 2)
    pi = 1.0

    @simd for i in 2:(rounds + 2)
        x = xs[i]
        pi += x / (2 * i - 1)
    end

    return pi*4
end

@static if abspath(PROGRAM_FILE) == @__FILE__
    rounds = parse(Int64, readchomp("rounds.txt"))
    print(f(rounds))
end
