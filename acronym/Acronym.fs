module Acronym

let abbreviate (phrase: string) =
    let words = phrase.Split([| ' '; ','; '_'; '-' |], System.StringSplitOptions.RemoveEmptyEntries)

    let acronym =
        words
        |> Seq.map Seq.head
        |> System.String.Concat

    acronym.ToUpperInvariant()
