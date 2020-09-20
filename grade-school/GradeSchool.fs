module GradeSchool

type School = Map<int, string list>

let empty: School = Map.empty

let grade (number: int) (school: School): string list =
    match Map.tryFind number school with
    | Some students -> students
    | None -> List.empty

let add (student: string) (g: int) (school: School): School =
    let studentsInGrade = grade g school
    let newStudentsInGrade = List.sort (student :: studentsInGrade)
    Map.add g newStudentsInGrade school

let roster (school: School): string list =
    let mapValues (m: Map<'k, 'v>) =
        m
        |> Map.toList
        |> List.map snd

    let grades = mapValues school
    if List.isEmpty grades then List.empty else (List.reduce List.append grades)
