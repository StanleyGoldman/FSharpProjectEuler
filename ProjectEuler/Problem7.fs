module Problem7

open System

let problem7 () = 
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
    |> Seq.skip 10000
    |> Seq.head

let problem7_correct () =
    
    let sieveOfErastosthenes testNumber =
        match testNumber with
        | 1.0 -> false
        | x when x < 4.0 -> false
        | x when x % 2.0 = 0.0 -> false
        | x when x < 9.0 -> true
        | x when x % 3.0 = 0.0 -> false
        | x ->
            
            let r = floor(sqrt(testNumber))
            let f = ref 5.0
            let result = ref None

            while ((!f <= r) && ((!result).IsNone)) do
                match x with
                | x when (x % !f) = 0.0 -> result := Some(false)
                | x when x % (!f + 2.0) = 0.0 -> result := Some(false)
                | _ -> f := !f + 6.0

            match !result with
            | Some(x) -> x
            | _ -> true

    Seq.initInfinite (fun index -> float(index + 2))
    |> Seq.filter(fun possiblePrime -> sieveOfErastosthenes possiblePrime)
    |> Seq.map(fun primeFloat -> int64(primeFloat))
    |> Seq.append [2L ; 3L]
    |> Seq.skip 10000
    |> Seq.head