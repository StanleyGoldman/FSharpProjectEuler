module Problem6

let problem6 () =
    
    let sumOfSquares maximum =
        {1L .. maximum}
        |> Seq.map(fun number -> number * number)
        |> Seq.sum

    let squareOfSums maximum =
        {1L .. maximum}
        |> Seq.sum
        |> fun result -> result * result

    (squareOfSums 100L) - (sumOfSquares 100L)

let problem6_correct () =

    let limit = 100L
    let sum = limit * (limit + 1L) / 2L
    let sum_sq = ((2L * limit) + 1L) * (limit + 1L) * limit / 6L
    int64(pown sum 2) - sum_sq