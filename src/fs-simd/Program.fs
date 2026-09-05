open System.IO
open System.Runtime.Intrinsics

[<EntryPoint>]
let main _ =
    if not Vector512.IsHardwareAccelerated then
        failwith "F# SIMD requires hardware-accelerated Vector512"
    let rounds = File.ReadAllText("rounds.txt").Trim() |> int
    let signs = Vector512.Create(-1.0, 1.0, -1.0, 1.0, -1.0, 1.0, -1.0, 1.0)
    let mutable denominators = Vector512.Create(3.0, 5.0, 7.0, 9.0, 11.0, 13.0, 15.0, 17.0)
    let mutable sums = Vector512<float>.Zero
    let mutable i = 0
    while rounds - i >= 8 do
        sums <- sums + signs / denominators
        denominators <- denominators + Vector512.Create(16.0)
        i <- i + 8
    let mutable pi = 1.0 + Vector512.Sum(sums)
    while i < rounds do
        let sign = if i % 2 = 0 then -1.0 else 1.0
        pi <- pi + sign / float (2 * i + 3)
        i <- i + 1
    printfn "%.16f" (4.0 * pi)
    0
