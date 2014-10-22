module Problem14

open Microsoft.FSharp.Collections

let problem14 () = 

    let collatzSequence start =
        start
        |> Seq.unfold(fun state ->
            match state with
            | 0L -> None
            | 1L -> Some(1L, 0L)
            | x when x % 2L = 0L -> Some(x, x/2L)
            | x when x % 2L = 1L -> Some(x, (3L * x) + 1L)
            | _ -> failwith "Invalid State")
        |> PSeq.toList

    let maxSeed, maxSequence, maxLength  = 
        {1L .. 999999L}
        |> PSeq.map(fun seed ->
            let collatz = collatzSequence seed
            (seed, collatz, collatz.Length))
        |> PSeq.maxBy(fun (_, _, length) -> length)

    sprintf "Seed: %i SequenceLength:%i" maxSeed maxLength