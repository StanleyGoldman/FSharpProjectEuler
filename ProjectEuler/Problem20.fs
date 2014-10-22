module Problem20

open System

let rec factorial =
    function
        | x when x > 1I -> x * (factorial (x - 1I))
        | _ -> 1I

let problem20 () =
    (factorial 100I).ToString().ToCharArray()
    |> Array.fold(fun acc character -> acc + Int32.Parse(character.ToString())) 0
    
    