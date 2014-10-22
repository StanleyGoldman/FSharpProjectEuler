module Problem2

let problem2 () =
    let fibonacciSeq withLimit =
        Seq.unfold (fun previousState ->
            let nMinusTwo, nMinusOne = previousState
            let nextValue = nMinusTwo + nMinusOne
            let nextState = (nMinusOne, nextValue)

            match nextValue < withLimit with
            | true -> Some(nextValue, nextState)
            | false -> None
        ) (0, 1)

    fibonacciSeq 4000000
    |> Seq.filter(fun x -> x % 2 = 0)
    |> Seq.sum