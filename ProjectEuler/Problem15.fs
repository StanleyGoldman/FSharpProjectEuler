module Problem15

let problem15 () =
    
    let maxX = 3
    let maxY = 2

    let possiblePositions point =
        let x, y = point

        match x = maxX, y = maxY with
        | false, false -> [(x + 1, y) ; (x, y + 1)]
        | true, false -> [(x, y + 1)]
        | false, true -> [(x + 1, y)]
        | true, true -> []

    let rec createPaths point =          
        possiblePositions point
        |> function 
            | n when n.Length = 0 -> seq [ seq [point]]
            | n ->
                Seq.map (fun possiblePoisition -> createPaths possiblePoisition) n
                |> Seq.concat
                |> Seq.map(fun possiblePositionList -> Seq.append [point] possiblePositionList )

    createPaths (0, 0)
    |> Seq.mapi(fun index path ->
        (*path
        |> Seq.iter(fun (x, y) ->
                let console = sprintf "(%i, %i)" x y
                System.Console.Write(console))*)
        
        let console = sprintf "%i Path Found" (index + 1)
        System.Console.WriteLine(console)

        path)
    |> Seq.length