module RobotSimulator

type Direction =
    | North
    | East
    | South
    | West

type Position = int * int

type Robot =
    { direction: Direction
      position: Position }

let create direction position =
    { direction = direction
      position = position }

type Turn =
    | Right
    | Left

let newDirection direction turn =
    match turn with
    | Right ->
        match direction with
        | North -> East
        | East -> South
        | South -> West
        | West -> North
    | Left ->
        match direction with
        | North -> West
        | East -> North
        | South -> East
        | West -> South

let turn robot turnDirection =
    { direction = newDirection robot.direction turnDirection
      position = robot.position }

let advancePosition position direction =
    let (x, y) = position
    match direction with
    | North -> (x, y + 1)
    | East -> (x + 1, y)
    | South -> (x, y - 1)
    | West -> (x - 1, y)

let advance robot =
    { direction = robot.direction
      position = advancePosition robot.position robot.direction }

let moveOne robot (instruction: char) =
    match instruction with
    | 'A' -> advance robot
    | 'R' -> turn robot Right
    | 'L' -> turn robot Left
    | _ -> failwith "Unexpected instruction"

let move (instructions: string) (robot: Robot): Robot = Seq.fold moveOne robot instructions
