module Bob

let (|Question|) (input: string) = input.TrimEnd().EndsWith("?")

let (|Yelling|) (input: string) =
    String.exists System.Char.IsLetter input
    && String.forall (fun c -> not (System.Char.IsLetter c) || System.Char.IsUpper c) input

let (|Nothing|) (input: string) = String.forall System.Char.IsWhiteSpace input

let response (input: string): string =
    match input with
    | Nothing true -> "Fine. Be that way!"
    | Question true & Yelling true -> "Calm down, I know what I'm doing!"
    | Question true -> "Sure."
    | Yelling true -> "Whoa, chill out!"
    | _ -> "Whatever."
