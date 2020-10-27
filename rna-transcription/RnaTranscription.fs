module RnaTranscription

let toRnaChar =
    function
    | 'G' -> 'C'
    | 'C' -> 'G'
    | 'T' -> 'A'
    | 'A' -> 'U'
    | _ -> failwith "Invalid nucleotide"

let toRna (dna: string): string = String.map toRnaChar dna
