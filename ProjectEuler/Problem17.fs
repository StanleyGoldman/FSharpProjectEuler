module Problem17

let problem17 () =
    
    let rec inWords = function
                        | 1000 -> "one thousand"
                        | 90 -> "ninety"
                        | 80 -> "eighty"
                        | 70 -> "seventy"
                        | 60 -> "sixty"
                        | 50 -> "fifty"
                        | 40 -> "forty"
                        | 30 -> "thirty"
                        | 20 -> "twenty"
                        | 19 -> "nineteen"
                        | 18 -> "eighteen"
                        | 17 -> "seventeen"
                        | 16 -> "sixteen"
                        | 15 -> "fifteen"
                        | 14 -> "fourteen"
                        | 13 -> "thirteen"
                        | 12 -> "twelve"
                        | 11 -> "eleven"
                        | 10 -> "ten"
                        | 9 -> "nine"
                        | 8 -> "eight"
                        | 7 -> "seven"
                        | 6 -> "six"
                        | 5 -> "five"
                        | 4 -> "four"
                        | 3 -> "three"
                        | 2 -> "two"
                        | 1 -> "one"
                        | n when n >= 100 && n < 1000 ->
                            match (n % 100) with
                            | 0 -> sprintf "%s hundred" (inWords (n/100))
                            | remainder -> sprintf "%s hundred and %s" (inWords (n/100)) (inWords remainder)
                        | n when n >= 20 && n < 100 ->
                            match (n % 10) with
                            | 0 -> sprintf "%s" (inWords n)
                            | remainder -> sprintf "%s %s" (inWords (n - remainder)) (inWords remainder)
                        | _ -> failwith "Invalid"


    {1 .. 1000}
    |> Seq.map(fun number ->
        let result = inWords number
        System.Console.WriteLine(result)
        result)
    |> String.concat ""
    |> fun x -> x.Replace(" ", "")
    |> fun x -> x.Length