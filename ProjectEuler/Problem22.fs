module Problem22

open System.Net

let problem22 () =

    let characterValue value =
        match value with
        | x when x.Equals("A")  || x.Equals("a") -> 1
        | x when x.Equals("B")  || x.Equals("b") -> 2
        | x when x.Equals("C")  || x.Equals("c") -> 3
        | x when x.Equals("D")  || x.Equals("d") -> 4
        | x when x.Equals("E")  || x.Equals("e") -> 5
        | x when x.Equals("F")  || x.Equals("f") -> 6
        | x when x.Equals("G")  || x.Equals("g") -> 7
        | x when x.Equals("H")  || x.Equals("h") -> 8
        | x when x.Equals("I")  || x.Equals("i") -> 9
        | x when x.Equals("J")  || x.Equals("j") -> 10
        | x when x.Equals("K")  || x.Equals("k") -> 11
        | x when x.Equals("L")  || x.Equals("l") -> 12
        | x when x.Equals("M")  || x.Equals("m") -> 13
        | x when x.Equals("N")  || x.Equals("n") -> 14
        | x when x.Equals("O")  || x.Equals("o") -> 15
        | x when x.Equals("P")  || x.Equals("p") -> 16
        | x when x.Equals("Q")  || x.Equals("q") -> 17
        | x when x.Equals("R")  || x.Equals("r") -> 18
        | x when x.Equals("S")  || x.Equals("s") -> 19
        | x when x.Equals("T")  || x.Equals("t") -> 20
        | x when x.Equals("U")  || x.Equals("u") -> 21
        | x when x.Equals("V")  || x.Equals("v") -> 22
        | x when x.Equals("W")  || x.Equals("w") -> 23
        | x when x.Equals("X")  || x.Equals("x") -> 24
        | x when x.Equals("Y")  || x.Equals("y") -> 25
        | x when x.Equals("Z")  || x.Equals("z") -> 26
        | _ -> failwith "Invalid Character"
    
    let webClient = new WebClient();
    let content = webClient.DownloadString("http://projecteuler.net/project/names.txt")

    content.Split(',')
    |> Array.toSeq
    |> Seq.mapi(fun index name -> name.Trim([|'"'|]))
    |> Seq.sort
    |> Seq.mapi(fun index name ->
        let nameValue =
            name.ToCharArray()
            |> Array.toSeq
            |> Seq.map(fun char -> characterValue (char.ToString()))
            |> Seq.sum

        (name, (index + 1), nameValue, nameValue * (index + 1)))
    |> Seq.sumBy(fun (name, index, nameValue, value) -> int64(value))
