﻿module TwoFer

let twoFer (input: string option): string = 
    match input with
    | Some -> sprintf "One for %s, one for me." input.Value
    | None -> "One for you, one for me."