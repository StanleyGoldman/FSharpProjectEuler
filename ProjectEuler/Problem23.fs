module Problem23

let problem23 () =

    let factors ofNumber =
        {2L .. (ofNumber/2L)}
        |> Seq.filter(fun testNumber -> ofNumber % testNumber = 0L)
        |> Seq.map(fun factor1 -> (factor1, (ofNumber / factor1)))
        |> Seq.takeWhile(fun (factor1, factor2) -> factor1 <= factor2)
        |> Seq.fold (fun acc (factor1, factor2) -> factor1 :: factor2 :: acc) []
        |> Seq.append [1L]
        |> Seq.toList

    let sumOfFactors ofNumber = 
        factors ofNumber
        |> Seq.sum

    let abundantNumbers = 
        Seq.initInfinite(fun i -> (int64) i)
        |> Seq.takeWhile(fun i -> i <= 28123L)
        |> Seq.filter(fun i -> (sumOfFactors i) > i)

    let threadList (thread:seq<int64>) =
        let head = Seq.head thread
        let tail = Seq.skip 1 thread

        tail
        |> Seq.map(fun i -> head + i)

    "boo"