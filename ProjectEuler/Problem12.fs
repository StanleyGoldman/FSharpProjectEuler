module Problem12

open Microsoft.FSharp.Collections

let problem12 () =

    let factors (ofNumber:int) =
        match ofNumber with
        | 1 -> [1]
        | _ ->
            {2 .. (ofNumber/2)}
            |> Seq.filter(fun testNumber -> ofNumber % testNumber = 0)
            |> Seq.map(fun factor1 -> (factor1, (ofNumber / factor1)))
            |> Seq.takeWhile(fun (factor1, factor2) -> factor1 < factor2)
            |> Seq.fold (fun acc (factor1, factor2) -> factor1 :: factor2 :: acc) []

    
    Seq.unfold(fun (index, value) -> 
        let nextState = (index + 1, value + index)
        Some(value, nextState)
    )(0, 0)
    |> Seq.skip 2
    |> Seq.find(fun triangle ->
        let factorsOfTriangleLength =
            factors triangle
            |> List.length

        System.Console.WriteLine("Factor Length: " + (factorsOfTriangleLength.ToString()) + " For Triangle: " + (triangle.ToString()))

        factorsOfTriangleLength > 500)
