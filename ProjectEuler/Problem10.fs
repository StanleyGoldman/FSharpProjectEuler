module Problem10

open Microsoft.FSharp.Collections

let problem10 () = 
    let testPrime (possiblePrime:int64) =
        let sqrRootOfPrime = int64(System.Math.Sqrt(float(possiblePrime)))
            
        {1L .. sqrRootOfPrime}
        |> Seq.forall(fun divisor -> 
            match divisor with
            | 1L -> true
            | x when divisor = possiblePrime -> true
            | _ -> possiblePrime % divisor > 0L)

    Seq.initInfinite (fun index -> int64(index + 2))
    |> Seq.filter(fun possiblePrime -> testPrime possiblePrime)
    |> Seq.takeWhile(fun prime -> prime < 2000000L)
    |> Seq.sum