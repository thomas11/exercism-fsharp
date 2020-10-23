// This is the file you need to modify for your own solution.
// The unit tests will use this code, and it will be used by the benchmark tests
// for the "Mine" row of the summary table.

// Remember to not only run the unit tests for this exercise, but also the
// benchmark tests using `dotnet run -c Release`.
// Please refer to the instructions for more information about the benchmark tests.

module TreeBuilding

open TreeBuildingTypes

type Tree =
    | Branch of int * Tree list
    | Leaf of int

let recordId t =
    match t with
    | Branch(id, c) -> id
    | Leaf id -> id

let isBranch t =
    match t with
    | Branch(id, c) -> true
    | Leaf id -> false

let children t =
    match t with
    | Branch(id, c) -> c
    | Leaf id -> []


// Latest benchmark (dotnet run -c Release)

// a0321ca: code is IMO much cleaner, but not much faster.
//|   Method |     Mean | Allocated |
//|--------- |---------:|----------:|
//| Baseline | 9.869 us |  14.76 KB |
//|     Mine | 7.231 us |  11.53 KB |

// e7e8acd
//|   Method |     Mean | Allocated |
//|--------- |---------:|----------:|
//| Baseline | 9.366 us |  14.76 KB |
//|     Mine | 5.982 us |   9.63 KB |


let private assertRootIsValid roots =
    let root = List.exactlyOne roots
    if root.ParentId <> 0 then invalidArg "records" "Root node has parent"


let private assertChildrenAreValid records =
    if List.exists (fun r -> r.RecordId <= r.ParentId) records then invalidArg "records" "invalid parent" |> ignore

    // A trick to check that all ids are consecutive, starting at 1: their sum must be the
    // triangular number x*(x-1)/2, with x being the highest record id. Saves another sort+iterate.
    let sumOfIds = List.sumBy (fun r -> r.RecordId) records
    let numRecords = (List.length records) + 1
    if sumOfIds <> (numRecords * (numRecords - 1)) / 2 then invalidArg "records" "non-consecutive ids" |> ignore


let rec private treeify byParents id =
    let parent = List.tryFind (fun (p, _) -> id = p) byParents
    match parent with
    | Some(_, c) -> Branch(id, List.map (fun r -> treeify byParents r.RecordId) c)
    | None -> Leaf id


let buildTree (records: Record list) =
    let (root, children) = List.partition (fun r -> r.RecordId = 0) records

    assertRootIsValid root
    assertChildrenAreValid children

    // [ (0, [1, 3, 5]), (1, [2, 4]), (3, [6])]
    let byParents =
        children
        |> List.groupBy (fun r -> r.ParentId)
        |> List.sortBy fst
        |> List.map (fun (p, c) -> (p, List.sortBy (fun r -> r.RecordId) c))

    match byParents with
    | [] -> Leaf 0
    | _ -> treeify byParents (fst byParents.[0])
