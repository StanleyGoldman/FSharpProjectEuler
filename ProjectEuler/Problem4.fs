module Problem4

let problem4 () =
    let isPalindrome (value: string) =
        match value.Length with
        | x when x % 2 = 1 -> (x-1)/2, (x-1)/2 + 1
        | x -> (x/2), (x/2)
        |> fun (pos1, pos2) ->
                    value.Substring(0, pos1), value.Substring(pos2)
        |> (fun (firstHalf, secondHalf) ->
        
            Array.rev (secondHalf.ToCharArray())
            |> (fun reversedcharArray -> new string(reversedcharArray))
            |> (fun reversedSecondHalf ->
                    firstHalf.Equals(reversedSecondHalf)))

    let productSeq (maximumInt:int) =
        Seq.unfold (fun state ->
            let mult1, mult2 = state
            let result = mult1 * mult2

            match state with
            | 100, _ -> None
            | _, 100 -> Some((result), ( (mult1 - 1) , (mult1 - 1)))
            | _ -> Some((result), ( mult1, (mult2 - 1)))
        ) (maximumInt, maximumInt)

    let largestPalindromeSoFar = ref None

    productSeq 999
    |> Seq.filter(fun (result) ->
        
        match !largestPalindromeSoFar with
        | Some(largestResult) when largestResult > result-> false
        | _ -> match isPalindrome (result.ToString()) with
                  | true ->
                        largestPalindromeSoFar := Some(result)
                        true
                  | _ -> false)
    |> Seq.max