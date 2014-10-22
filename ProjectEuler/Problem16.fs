module Problem16

open System

let problem16 () =
    (pown 2I 1000).ToString().ToCharArray()
    |> Array.fold(fun state value -> state + Numerics.BigInteger.Parse(value.ToString())) 0I