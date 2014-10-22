module Problem19

let problem19 () =
    
    Seq.unfold (fun state ->
        match state with
        | _, _, 2001, _ -> None
        | date, month, year, day ->
            
            let leapYear =
                match year with
                | n when n % 4 = 0 && not(n % 100 = 0) -> true
                | n when n % 4 = 0 && n % 100 = 0 && n % 400 = 0 -> true
                | _ -> false

            let daysInMonth =
                match month, leapYear with
                | 9, _
                | 4, _
                | 6, _
                | 11, _ -> 30
                | 2, false -> 28
                | 2, true -> 29
                | _ -> 31

            let nextState =
                match daysInMonth - date, month % 12 with
                | 0, 0 -> (1, 1, year+1, (day % 7) + 1)
                | 0, _ -> (1, month + 1, year, (day % 7) + 1)
                | x, _ -> (date + 1, month, year, (day % 7) + 1)

            Some(state, nextState)
        )(1, 1, 1900, 1)
    |> Seq.filter(fun (date, month, year, day) -> date = 1 && day = 7 && year > 1900)
    |> Seq.length
