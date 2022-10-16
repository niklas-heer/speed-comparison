include("leibniz.jl")

function mainjl()
    fp = fopen(m"rounds.txt", m"r")
    buf = MallocString(undef, 16)
    StaticTools.fread!(buf, fp)
    fclose(fp)
    rounds = parse(Int64, buf)
    free(buf)
    
    res = f(rounds)
    printf(c"%.15f", res)
    return 0
end

