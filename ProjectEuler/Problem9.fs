module Problem9

open System
open Microsoft.FSharp.Core.Operators

let problem9 () =
    Seq.initInfinite(fun index -> 
        let n = index + 2
        {1 .. (n - 1)}
        |> Seq.map(fun m -> 
            let a = (pown n 2) - (pown m 2)
            let b = 2 * m * n
            let c = (pown n 2) + (pown m 2)

            let tripletSumEqualsThousand = (a + b + c) = 1000
            (a , b,  c), tripletSumEqualsThousand)
        |> Seq.toList)
    |> Seq.choose(fun iterSet ->
        iterSet
        |> List.tryFind(fun (tripletSet, equals1000) -> equals1000 ))
    |> Seq.map(fun ((a, b, c), _) ->  a * b * c)
    |> Seq.head

let problem9_correct () =
    let s = 1000.0
    let s2 = s / 2.0
    let mLimit =
        ceil(sqrt(s2)) - 1.0
        |> int

    let result = ref None
    let i = ref 2

    while (!i < mLimit) && ((!result).IsNone) do
        ignore