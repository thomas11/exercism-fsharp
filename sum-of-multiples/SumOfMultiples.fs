module SumOfMultiples

let multiples upperBound number =
    1
    |> Seq.unfold (fun x ->
        let multiple = x * number
        if multiple >= upperBound then None else Some(multiple, x + 1))

let sum (numbers: int list) (upperBound: int): int =
    numbers
    |> List.filter (fun x -> x > 0)
    |> Seq.collect (multiples upperBound)
    |> Seq.distinct
    |> Seq.sum
