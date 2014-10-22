module Problem40

open System

let problem40 () =

    let irrationalNumericSequence () =
        Seq.initInfinite(fun index -> (index + 1).ToString().ToCharArray())
        |> Seq.collect(fun characters -> Array.toSeq characters)

    let numericSequenceAtPosition position =
        irrationalNumericSequence ()
        |> Seq.skip (position - 1)
        |> Seq.head
        |> fun char -> int64(char.ToString())

    numericSequenceAtPosition 1 *
    numericSequenceAtPosition 10 *
    numericSequenceAtPosition 100 *
    numericSequenceAtPosition 1000 *
    numericSequenceAtPosition 10000 *
    numericSequenceAtPosition 100000 *
    numericSequenceAtPosition 1000000