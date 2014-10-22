module Problem25

open System

let problem25 () =
    let fibonacciSeq withLimit =
        Seq.unfold (fun previousState ->
            let nMinusTwo, nMinusOne = previousState
            let nextValue = nMinusTwo + nMinusOne

            match nextValue.ToString().Length <= withLimit with
            | true ->
                System.Console.WriteLine("Length: " + (nextValue.ToString().Length.ToString()) + " Smaller Fib: " + (nextValue.ToString()))
                Some(nextValue, (nMinusOne, nextValue))
            | false -> None
        ) (0N, 1N)
        

    (fibonacciSeq 1000
    |> Seq.findIndex(fun fib -> fib.ToString().Length = 1000)) + 2

