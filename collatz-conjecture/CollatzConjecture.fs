module CollatzConjecture

let next n =
    if n % 2 = 0 then n / 2 else 3 * n + 1

let collatzSeq number =
    number
    |> Seq.unfold (fun n ->
        if n = 1 then None else Some(n, next n))

let steps (number: int): int option =
    if number > 0 then
        number
        |> collatzSeq
        |> Seq.length
        |> Some
    else
        None
