open System
open System.IO
let rounds = File.ReadAllText("rounds.txt", System.Text.Encoding.UTF8) |> int
let pi = 
    let rec loop (i:int) (rounds:int) (pi:double) = 
        if i >= rounds + 2 then pi
        else 
            loop (i + 1) rounds (-(pi + (1.0 / (double (2 * i - 1)))))
    -4.0 * (loop 0 rounds 1.0)
printfn "%A" pi
