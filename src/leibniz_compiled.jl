function mainjl()
    f = fopen(m"rounds.txt", m"r")
    buf = MallocString(undef, 16)
    StaticTools.fread!(buf, f)
    rounds = parse(Int64, buf)
    x = 1.0
    pi = 1.0

    for i in 2:(rounds + 2)
        x *= -1
        pi += x / (2 * i - 1)
    end
    free(buf)
    printf(c"%.15f", pi*4)
    return 0
end

