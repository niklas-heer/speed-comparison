using StaticCompiler, StaticTools

include("leibniz.jl")

function mainjl()
    fp = fopen(m"rounds.txt", m"r")
    buf = MallocString(undef, 16)
    StaticTools.fread!(buf, fp)

    free(buf)
    fclose(fp)

    rounds = parse(Float32, buf)

    res = Float64(f(rounds))  # if called with rounds using Int64 then 5x slower, avoidable but maybe bug in StaticCompiler
    printf(c"%.15f", res)
    return 0
end
