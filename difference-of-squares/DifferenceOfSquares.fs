module DifferenceOfSquares

let squareOfSum (number: int): int =
    let sum = seq { 1 .. number } |> Seq.sum
    sum * sum

let sumOfSquares (number: int): int = seq { 1 .. number } |> Seq.sumBy (fun n -> n * n)

let differenceOfSquares (number: int): int = (squareOfSum number) - (sumOfSquares number)
