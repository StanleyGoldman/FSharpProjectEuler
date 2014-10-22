module Problem1

let problem1 () =
    {1 .. 999}
    |> Seq.filter (fun x -> (x % 3 = 0) || (x % 5 = 0))
    |> Seq.sum

let problem1_correct () =
    let target = 999

    let sumDivisibleBy value =
        let p = target / value
        value * (p * (p + 1)) / 2

    (sumDivisibleBy 3) + (sumDivisibleBy 5) - (sumDivisibleBy 15)