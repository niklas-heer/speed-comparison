open System.IO

[<EntryPoint>]
let main _ =
    let rounds = File.ReadAllText("rounds.txt").Trim() |> int

    let mutable pi = 1.0
    let mutable x = 1.0

    for i in 2 .. rounds + 1 do
        x <- -x
        pi <- pi + (x / float (2 * i - 1))

    pi <- pi * 4.0
    printfn "%.16f" pi
    0
