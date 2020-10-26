module SimpleLinkedList

type LinkedList =
    | Head of int * LinkedList
    | Nil

let nil = Nil

let create x n = Head(x, n)

let isNil x = x = Nil

let next =
    function
    | Head(_, n) -> n
    | Nil -> Nil

let datum =
    function
    | Head(d, _) -> d
    | Nil -> failwith ("Nil has no datum")

let toList x =
    x
    |> Seq.unfold (fun elem ->
        if isNil elem then None else Some(elem |> datum, elem |> next))
    |> Seq.toList

let fromList xs = List.foldBack create xs Nil

// From bugra's https://exercism.io/tracks/fsharp/exercises/simple-linked-list/solutions/7c42c9740d0c44e1a288808872a5d24a
let reverse x =
    let rec rev acc ll =
        match ll with
        | Nil -> acc
        | Head(head, tail) -> rev (create head acc) tail

    rev Nil x
