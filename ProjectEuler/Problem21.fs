module Problem21

let problem21 () =

    let factors ofNumber =
        {2L .. (ofNumber/2L)}
        |> Seq.filter(fun testNumber -> ofNumber % testNumber = 0L)
        |> Seq.map(fun factor1 -> (factor1, (ofNumber / factor1)))
        |> Seq.takeWhile(fun (factor1, factor2) -> factor1 <= factor2)
        |> Seq.fold (fun acc (factor1, factor2) -> factor1 :: factor2 :: acc) []
        |> Seq.append [1L]
        |> Seq.toList
        
    let sumProperDivisors =        
        {1L .. 10000L}
        |> Seq.map (fun index -> (index, factors index))
        |> Seq.map (fun (index, factors) -> (index, List.sum factors))
        |> Seq.toList
        
    sumProperDivisors
    |> List.filter (fun (a, b) -> 
            sumProperDivisors
            |> List.filter (fun (a1, b1) -> 
                not(a = a1) && (a = b1) && (b = a1)
                )
            |> function
                | n when n.Length > 1 -> failwith("More than one")
                | [] -> false
                | n -> true)
    |> List.fold (fun acc (val1, val2) -> val1 :: val2 :: acc) []
    |> Seq.distinct
    |> Seq.sum
