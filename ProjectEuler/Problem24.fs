module Problem24

open System.Collections.Generic

let problem24 () =

    let itemsWithoutIndex items index = 
        items
        |> List.mapi (fun i list -> match i = index with
                                    | true -> None
                                    | _ -> Some(list))
        |> List.choose(fun i -> i)

    let permutations = new List<string>()

    let rec permute result (items: string list) =
        match items.IsEmpty with
        | false ->
            items
            |> List.mapi(fun index value -> permute (List.append result [value]) (itemsWithoutIndex items index))
            |> ignore
        | _ -> 
            result
            |> String.concat ""
            |> permutations.Add


    Seq.initInfinite (fun i -> i.ToString())
    |> Seq.take 10 
    |> Seq.toList
    |> permute []

    permutations
    |> Seq.sort
    |> Seq.skip 999999
    |> Seq.take 1
    |> Seq.head