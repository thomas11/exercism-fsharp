module OcrNumbers

open System

let hasCorrectNumberOfLines input = (List.length) input % 4 = 0

let hasCorrectRowLength input = not (List.exists (fun s -> (String.length s) % 3 <> 0) input)

let assertDimensions input = hasCorrectNumberOfLines input && hasCorrectRowLength input

let tokenizeRows input =
    input
    |> List.chunkBySize 4
    |> List.map (Seq.take 3 >> Seq.toList)

let parseOneDigit ((s1, s2, s3): string*string*string) =
    match (s1, s2, s3) with
    | ( " _ ",
        "| |",
        "|_|" ) -> "0"
    | ( "   ",
        "  |",
        "  |" ) -> "1"
    | ( " _ ",
        " _|",
        "|_ " ) -> "2"
    | ( " _ ",
        " _|",
        " _|" ) -> "3"
    | ( "   ",
        "|_|",
        "  |" ) -> "4"
    | ( " _ ",
        "|_ ",
        " _|" ) -> "5"
    | ( " _ ",
        "|_ ",
        "|_|" ) -> "6"
    | ( " _ ",
        "  |",
        "  |" ) -> "7"
    | ( " _ ",
        "|_|",
        "|_|" ) -> "8"
    | ( " _ ",
        "|_|",
        " _|" ) -> "9"
    | _         -> "?"

let parseOneDigitTuple ((r1, r2, r3): char[]*char[]*char[]) =
    parseOneDigit (String.Concat r1, String.Concat r2, String.Concat r3)

let parseRow (input: string list) =
    let rowsInChunks =
        input
        |> List.map (Seq.chunkBySize 3)
    Seq.zip3 rowsInChunks.[0] rowsInChunks.[1] rowsInChunks.[2]
    |> Seq.map parseOneDigitTuple

let parseValidInput (input: string list) =
    input
    |> tokenizeRows
    |> List.map (parseRow >> String.concat "")
    |> String.concat ","

let convert input =
    match assertDimensions input with
    | false -> None
    | true -> Some(parseValidInput input)
