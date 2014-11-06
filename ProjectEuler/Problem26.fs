module Problem26
open System.Collections.Generic

type SuffixTreeNode = { mutable Content : string; Children : List<SuffixTreeNode> }

type SuffixTreeRoot = { Children : List<SuffixTreeNode> }

let problem26 () =
    
    let splitStringSuffixes (input:string) =
        Seq.unfold(fun (rem : string) ->
            match rem.Length with
            | x when x = 0 -> None
            | _ -> Some(rem, rem.Substring(1))
        ) input
        |> Seq.toList

    let splitStringPrefixes (input:string) =
        Seq.unfold(fun (rem : int) ->
            match rem with
            | x when x = 0 -> None
            | _ -> Some(input.Substring(0, rem), rem - 1)
        ) input.Length
        |> Seq.toList

    let createSuffixTreeNode content = {Content = content ; Children = new List<SuffixTreeNode>()}

    let stringStartsWithPrefix (haystack : string) needle =
        splitStringPrefixes needle
        |> Seq.tryPick(fun prefix -> match haystack.StartsWith(prefix) with
                                     | true -> Some(prefix)
                                     | false -> None)

    let rec findBestMatchingNode content (nodes : SuffixTreeNode seq) =
        let result =
            nodes
            |> Seq.tryPick(fun node ->
                    match stringStartsWithPrefix node.Content content with
                    | Some(prefix) ->
                        let remainingString = content.Substring(prefix.Length)
                        let childrenCount = node.Children.Count

                        match remainingString.Length > 0, childrenCount with
                        | true, 0 -> Some(node, prefix, remainingString)
                        | _ -> match findBestMatchingNode remainingString node.Children with
                                | Some(node, prefix, remainingString) -> Some(node, prefix, remainingString)
                                | _ -> None
                    | _ -> None)

        result

    let splitTreeNode (node : SuffixTreeNode) (prefix : string) =       
        let childNode = 
            node.Content.Substring(prefix.Length)
            |> createSuffixTreeNode

        childNode.Children.AddRange(node.Children)
        node.Children.Clear()
        node.Children.Add childNode
        node.Content <- prefix
  (*
    let buildSuffixTree (input:string) = 
        let suffixTreeRoot = {Children = new List<SuffixTreeNode>()}
    
        splitStringSuffixes input
        |> Seq.iter(fun item ->
            match suffixTreeRoot.Children.Count with
            | 0 -> suffixTreeRoot.Children.Add(createSuffixTreeNode item)
            | _ ->
                match findBestMatchingNode item suffixTreeRoot.Children with
                | Some(node, prefix) ->
                    match prefix = item with
                    | true -> ()
                    | false -> 
                        splitTreeNode node prefix
                        node.Children.Add(createSuffixTreeNode (item.Substring(prefix.Length)))
                | None -> suffixTreeRoot.Children.Add(createSuffixTreeNode item)
                | _ -> ())
    
        suffixTreeRoot
    //let prefixes = splitStringPrefixes "dogsdogs"
    //let suffixes = splitStringSuffixes "dogsdogs"

    let suffixTree = buildSuffixTree "missis"
        *)

    //let testOne = stringStartsWithPrefix "anana" "bana"
    
    Seq.initInfinite(fun i -> (decimal) i + 101M)
    |> Seq.map(fun i -> (i, 1M/i))
    |> Seq.takeWhile(fun (i, _) -> i <= 200M)
    |> Seq.iter(fun (i, j) -> System.Console.WriteLine(i.ToString() + "\t" + j.ToString()))

    "boo"