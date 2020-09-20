module BeerSong

let bottlesOfBeer (n: int) =
    match n with
    | 0 -> "no more bottles of beer"
    | 1 -> "1 bottle of beer"
    | x when x > 1 -> sprintf "%i bottles of beer" x
    | _ -> failwithf "Cannot have %i bottles" n

let regularVerse (n: int) =
    let b = bottlesOfBeer n

    let pronoun =
        if n = 1 then "it" else "one"
    [ sprintf "%s on the wall, %s." b b
      sprintf "Take %s down and pass it around, %s on the wall." pronoun (bottlesOfBeer (n - 1)) ]

let reciteVerse (bottles: int): string list =
    match bottles with
    | 0 ->
        [ "No more bottles of beer on the wall, no more bottles of beer."
          "Go to the store and buy some more, 99 bottles of beer on the wall." ]
    | _ -> regularVerse bottles

let recite (startBottles: int) (takeDown: int) =
    let verses =
        seq {
            for n in startBottles .. -1 .. (startBottles - takeDown + 1) do
                yield! reciteVerse n
                yield ""
        }
        |> Seq.toList
    List.take ((List.length verses) - 1) verses
