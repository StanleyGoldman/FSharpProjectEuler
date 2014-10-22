module Problem15

let problem15 () =
    
    let maxX = 12
    let maxY = 12

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
        
        //let console = sprintf "%i Path Found" (index + 1)
        //System.Console.WriteLine(console)

        path)
    |> Seq.length

let problem15_attempt2 () =
    let maxX = 20
    let maxY = 20

    let hasRightChild coordinate =
        match coordinate with
        | x , _ when x < maxX -> true
        | _ -> false
        
    let hasBottomChild coordinate =
        match coordinate with
        | _ , y when y < maxY -> true
        | _  -> false

    let rec countPaths coordinate =
        match hasRightChild(coordinate), hasBottomChild(coordinate), coordinate with
        | true, true, (x,y) -> countPaths(x + 1, y) + countPaths(x, y + 1)
        | true, false, (x,y) -> countPaths(x + 1, y)
        | false, true, (x,y) -> countPaths(x, y + 1)
        | false, false, (x,y) -> 1I
     
    countPaths (0, 0)

let problem15_attempt3 () =
    let maxX = 20
    let maxY = 20

    let matrix = Array2D.create 21  21 1I

    for row = ((Array2D.length1 matrix) - 2) downto 0 do
        for column = ((Array2D.length2 matrix) - 2) downto 0 do
            matrix.[row, column] <- matrix.[row + 1, column] + matrix.[row, column + 1] 

    matrix.[0, 0]