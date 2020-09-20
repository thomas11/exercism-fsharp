module KindergartenGarden

type Plant =
    | Grass
    | Clover
    | Violets
    | Radishes

let plantFromCode code =
    match code with
    | 'G' -> Grass
    | 'C' -> Clover
    | 'V' -> Violets
    | 'R' -> Radishes
    | _ -> failwith (sprintf "Did not expect plant code %c" code)

let kids =
    [ "Alice"; "Bob"; "Charlie"; "David"; "Eve"; "Fred"; "Ginny"; "Harriet"; "Ileana"; "Joseph"; "Kincaid"; "Larry" ]

let parseCodesFromRow (student: string) (row: string) =
    let indexForStudent = 2 * (List.findIndex (fun s -> s = student) kids)
    [| row.[indexForStudent]
       row.[indexForStudent + 1] |]

let plants (diagram: string) (student: string) =
    diagram.Split('\n')
    |> Array.map (parseCodesFromRow student)
    |> Array.collect id
    |> Array.map plantFromCode
    |> Array.toList
