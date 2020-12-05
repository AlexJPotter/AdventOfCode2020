module Day03

open FileHelpers

let treeIndicator = '#'

type WorldMap = char [] []

let getWorldMap () : WorldMap =
    readLinesFromFile "Day03/input.txt" |> Array.map (fun line -> seq line |> Seq.toArray)

type Coordinate = { x: int; y: int }

let origin : Coordinate = { x = 0; y = 0 }

let containsTree (worldMap: WorldMap, coordinate: Coordinate) =
    let row = worldMap.[coordinate.y]
    row.[coordinate.x % row.Length] = treeIndicator

type Trajectory = { right: int; down: int }

let rec getTotalTreesHit (worldMap: WorldMap, currentCoordinate: Coordinate, trajectory: Trajectory, treesHitSoFar: int) =
    let newCoordinate = { x = currentCoordinate.x + trajectory.right; y = currentCoordinate.y + trajectory.down }

    if newCoordinate.y >= worldMap.Length then
        treesHitSoFar
    else
        getTotalTreesHit (
            worldMap,
            newCoordinate,
            trajectory,
            treesHitSoFar + (if containsTree (worldMap, newCoordinate) then 1 else 0)
        )

let partOne () =
    let trajectory = { right = 3; down = 1 }
    let worldMap = getWorldMap()
    getTotalTreesHit (worldMap, origin, trajectory, 0)

let partTwo () =
    let trajectories = [|
        { right = 1; down = 1 };
        { right = 3; down = 1 };
        { right = 5; down = 1 };
        { right = 7; down = 1 };
        { right = 1; down = 2 };
    |]

    let worldMap = getWorldMap()

    trajectories
        |> Seq.toArray
        |> Array.map (fun trajectory -> getTotalTreesHit (worldMap, origin, trajectory, 0))
        |> Array.reduce (fun a b -> a * b)

