module Problem28

type Direction =
    | Up
    | Down
    | Left
    | Right

let problem28 () =      
    
    let gridSize = 5

    let result =
        Array.init gridSize (fun index -> Array.create gridSize 0)

    let center = gridSize / 2

    let isDiagonalPoint =
        function 
        | y, x when x = y -> true
        | y, x when x + y = (gridSize - 1) -> true
        | _ -> false

    let rec fillSpiral (y, x) direction value =
        match y, x with
        | 0, x1 when x1 = gridSize -> ()
        | _ ->
            result.[y].[x] <- value

            let newPoint =
                match direction with
                | Up -> (y - 1, x)
                | Down -> (y + 1, x)
                | Left -> (y, x - 1)
                | Right -> (y, x + 1)

            let newDirection =
                match isDiagonalPoint newPoint, direction, y = 0, isDiagonalPoint (y, x) with
                | true, Up, _, _ -> Right
                | true, Down, _, _ -> Left
                | true, Left, _, _ -> Up
                | true, Right, true, false -> Down
                | _, current, _, _ -> current

            fillSpiral newPoint newDirection (value + 1)

    result.[center].[center] <- 1
    fillSpiral (center, center + 1) Down 2


    ""
    