module Problem37

open System

let problem37 () =
    
    let testPrime (possiblePrime:int64) =
        match possiblePrime with
        | 1L -> false
        | _ ->
            let sqrRootOfPrime = int64(System.Math.Sqrt(float(possiblePrime)))
            
            {2L .. sqrRootOfPrime}
            |> Seq.forall(fun divisor -> 
                match divisor with
                | x when divisor = possiblePrime -> true
                | _ -> possiblePrime % divisor > 0L)

    let testTruncatablePrime prime =
        let primeString = prime.ToString()

        let leftTruncates =
            primeString
            |> Seq.unfold(fun state ->
                match state.Length with
                | 0 -> None
                | x -> Some(state, state.Substring(0, x - 1)))
            |> Seq.skip 1

        let rightTruncates =
            primeString
            |> Seq.unfold(fun state ->
                match state.Length with
                | 0 -> None
                | x -> Some(state, state.Substring(1, x - 1)))
            |> Seq.skip 1

        [ rightTruncates ; leftTruncates ]
        |> Seq.concat
        |> Seq.distinct
        |> Seq.map(fun truncatedPrimeString -> Int64.Parse(truncatedPrimeString))
        |> Seq.forall(fun truncatedPrime -> testPrime truncatedPrime)

    Seq.initInfinite(fun i -> int64(i) + 10L)
    |> Seq.filter(fun number -> testPrime number)
    |> Seq.filter(fun number -> testTruncatablePrime number)
    |> Seq.take 11
    |> Seq.sum
