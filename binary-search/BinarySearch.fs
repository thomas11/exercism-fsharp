module BinarySearch

let find input value =
    let len = Array.length input

    let rec findRec low high =
        let middle = (low + high) / 2
        if input.[middle] = value then Some middle
        elif low = high then None
        elif input.[middle] < value then findRec (middle + 1) high
        else findRec low middle

    if len = 0 then None else findRec 0 (len-1)
