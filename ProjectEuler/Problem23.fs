module Problem23

let problem23 () =

    let factors ofNumber =
        {2L .. (ofNumber/2L)}
        |> Seq.filter(fun testNumber -> ofNumber % testNumber = 0L)
        |> Seq.map(fun factor1 -> (factor1, (ofNumber / factor1)))
        |> Seq.takeWhile(fun (factor1, factor2) -> factor1 <= factor2)
        |> Seq.fold (fun acc (factor1, factor2) -> factor1 :: factor2 :: acc) []
        |> Seq.distinct
        |> Seq.append [1L]
        |> Seq.toList

    let sumOfFactors ofNumber = 
        factors ofNumber
        |> Seq.sum

    let abundantNumbers = 
        Seq.initInfinite(fun i -> (int64) i + 2L)
        |> Seq.takeWhile(fun i -> i <= 28123L)
        |> Seq.filter(fun i -> (sumOfFactors i) > i)
        |> Seq.toList

    let sumsOfAbundantNumbers =
        abundantNumbers
        |> Seq.mapi(fun index value ->
            abundantNumbers
            |> Seq.skip index
            |> Seq.map (fun value2 -> value + value2))
        |> Seq.concat
        |> Seq.filter(fun i -> i <= 28123L)
        |> Seq.sort
        |> Seq.distinct

    let nonSumsOfAbundantNumbers = 
        sumsOfAbundantNumbers
        |> Seq.append [0L]
        |> Seq.pairwise
        |> Seq.map(fun (sum1,sum2) ->
                [sum1 .. sum2]
                |> Seq.filter(fun i -> not(i=sum1 || i=sum2))
                |> Seq.toList)
        |> Seq.concat

    nonSumsOfAbundantNumbers
    |> Seq.sum