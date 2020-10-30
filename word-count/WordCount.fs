module WordCount

let private separators =
    seq {
        yield! seq { 0 .. 38 }
        yield! seq { 40 .. 47 }
        yield! seq { 58 .. 64 }
        yield! seq { 91 .. 96 }
        yield! seq { 123 .. 127 }
    }
    |> Seq.map char
    |> Seq.toArray

let private addOrInc word map =
    match Map.tryFind word map with
    | Some count -> Map.add word (count + 1) map
    | None -> Map.add word 1 map

let private preprocess (word: string) = word.ToLowerInvariant().Trim('\'')

let countWords (phrase: string) =
    let initMap = new Map<string, int>([])

    let words = phrase.Split(separators, System.StringSplitOptions.RemoveEmptyEntries)

    let countFolder (counts: Map<string, int>) (word: string) = addOrInc (preprocess word) counts

    Array.fold countFolder initMap words
