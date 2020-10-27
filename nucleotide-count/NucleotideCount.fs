module NucleotideCount

let nucleotideCounts (strand: string): Option<Map<char, int>> =
    let zeroCounts =
        new Map<char, int>([ 'A', 0
                             'C', 0
                             'G', 0
                             'T', 0 ])

    let inc (counts: Map<char, int>) c = Map.add c (counts.[c] + 1) counts

    try
        Some(Seq.fold inc zeroCounts strand)
    with :? System.Collections.Generic.KeyNotFoundException -> None
