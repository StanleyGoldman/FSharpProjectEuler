module Problem5

open System

let problem5 () =
    let evenlyDivisibleByAllUpTo limit number =
        {limit .. -1 .. 1}
        |> Seq.forall(fun testNumber -> number % testNumber = 0)

    Seq.initInfinite(fun i -> (i + 1) * 20)
    |> Seq.find(fun testIndex ->evenlyDivisibleByAllUpTo 20 testIndex)

let problem5_optimized () =
    let factors (ofNumber:int) =
        match ofNumber with
        | 1 -> [1]
        | _ ->
            {2 .. (ofNumber/2)}
            |> Seq.filter(fun testNumber -> ofNumber % testNumber = 0)
            |> Seq.map(fun factor1 -> (factor1, (ofNumber / factor1)))
            |> Seq.takeWhile(fun (factor1, factor2) -> factor1 < factor2)
            |> Seq.fold (fun acc (factor1, factor2) ->
                                        factor1 :: factor2 :: acc) []

    let factorsOfTwenty = 
        factors 20
        |> Seq.toList

    let listOfNumbersToBeDivisibleBy =
        {19 .. -1 .. 2}
        |> Seq.filter(fun number ->
            factorsOfTwenty
            |> List.exists(fun factor -> number = factor)
            |> not)
        |> Seq.toList

    let evenlyDivisibleByAllUpTo number =
        listOfNumbersToBeDivisibleBy
        |> List.forall(fun testNumber -> number % testNumber = 0)

    Seq.initInfinite(fun i -> ((i + 1) * 20) + (19 * 20))
    |> Seq.find(fun testIndex -> evenlyDivisibleByAllUpTo testIndex)

let problem5_correct () =
    
    let primesWithLimit limit =
        let testPrime (possiblePrime:float) =
            let sqrRootOfPrime = sqrt(possiblePrime)
            
            {2.0 .. sqrRootOfPrime}
            |> Seq.forall(fun divisor -> 
                match divisor with
                | 1.0 -> true
                | x when divisor = possiblePrime -> true
                | _ -> possiblePrime % divisor > 0.0)

        {2.0 .. limit}
        |> Seq.filter(fun index -> testPrime index)

    let computeLimit = sqrt(20.0)

    primesWithLimit 20.0
    |> Seq.fold(fun state prime ->
        
        let exponent =
            match prime < computeLimit with
            | true -> floor( (log(20.0) / log(prime)) )
            | _ -> 1.0

        state * (float(prime) ** exponent)) 1.0
    |> int64