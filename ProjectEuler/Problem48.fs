module Problem48

open System

let problem48 () =
    1000I
    |> Seq.unfold(fun state -> 
        match state with
        | x when x = 0I -> None
        | _ -> Some(pown state (int32 state), state-1I))
    |> Seq.sum
    |> fun x -> x.ToString()
    |> fun x -> x.Substring(x.Length - 10)