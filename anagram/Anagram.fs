module Anagram

let private charsInString (str: string) =
    str.ToLowerInvariant()
    |> List.ofSeq
    |> List.sort

let findAnagrams (sources: string list) (target: string) =
    let targetChars = charsInString target

    let isAnagram (word: string) =
        (word.ToLowerInvariant()) <> (target.ToLowerInvariant()) && (charsInString word) = targetChars

    List.filter isAnagram sources
