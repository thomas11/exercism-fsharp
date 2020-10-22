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
// My code is IMO much cleaner, but not much faster.
//
//|   Method |     Mean | Allocated |
//|--------- |---------:|----------:|
//| Baseline | 9.869 us |  14.76 KB |
//|     Mine | 7.231 us |  11.53 KB |

let rec treeify byParents id =
    let parent = List.tryFind (fun (p, _) -> id = p) byParents
    match parent with
    | Some(id, c) -> Branch(id, List.map (fun r -> treeify byParents r.RecordId) c)
    | None -> Leaf id

let assertInputIsValid records =
    let (separateRoot, records') = List.partition (fun r -> r.RecordId = 0) records

    let root = List.exactlyOne separateRoot
    if root.ParentId <> 0 then invalidArg "records" "Root node has parent"

    if List.exists (fun r -> r.RecordId <= r.ParentId) records' then invalidArg "records" "invalid parent" |> ignore

    // A trick to check that all ids are consecutive, starting at 1: their sum must be the
    // triangular number x*(x-1)/2, with x being the highest record id. Saves another sort+iterate.
    let sumOfIds = List.sumBy (fun r -> r.RecordId) records'
    let numRecords = List.length records
    if sumOfIds <> (numRecords * (numRecords - 1)) / 2 then invalidArg "records" "non-consecutive ids" |> ignore

    records'

let buildTree (records: Record list) =
    let records' = assertInputIsValid records

    // [ (0, [1, 3, 5]), (1, [2, 4]), (3, [6])]
    let byParents =
        records'
        |> List.groupBy (fun r -> r.ParentId)
        |> List.sortBy fst
        |> List.map (fun (p, c) -> (p, List.sortBy (fun r -> r.RecordId) c))

    match byParents with
    | [] -> Leaf 0
    | _ -> treeify byParents (fst byParents.[0])


// The pre-existing method replaced by the above code
let buildTreeOld records =
    let records' = List.sortBy (fun x -> x.RecordId) records

    if List.isEmpty records' then
        failwith "Empty input"
    else
        let root = records'.[0]
        if (root.ParentId = 0 |> not) then
            failwith "Root node is invalid"
        else if (root.RecordId = 0 |> not) then
            failwith "Root node is invalid"
        else
            let mutable prev = -1
            let mutable leafs = []

            for r in records' do
                if (r.RecordId <> 0 && (r.ParentId > r.RecordId || r.ParentId = r.RecordId)) then
                    failwith "Nodes with invalid parents"
                else if r.RecordId <> prev + 1 then
                    failwith "Non-continuous list"
                else
                    prev <- r.RecordId
                    if (r.RecordId = 0)
                    then leafs <- leafs @ [ (-1, r.RecordId) ]
                    else leafs <- leafs @ [ (r.ParentId, r.RecordId) ]

            let root = leafs.[0]

            let grouped =
                leafs
                |> List.groupBy fst
                |> List.map (fun (x, y) -> (x, List.map snd y))

            let parents = List.map fst grouped
            let map = grouped |> Map.ofSeq

            let rec helper key =
                if Map.containsKey key map
                then Branch(key, List.map (fun i -> helper i) (Map.find key map))
                else Leaf key

            let root = helper 0
            root
