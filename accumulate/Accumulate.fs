module Accumulate

let accumulate (func: 'a -> 'b) (input: 'a list): 'b list =
    let rec mapImp f acc =
        function
        | [] -> acc
        | h :: t -> mapImp f (f h :: acc) t
    mapImp func [] input |> List.rev
