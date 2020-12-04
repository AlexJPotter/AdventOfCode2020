module Day03

open FileHelpers

let treeIndicator = '#'

type WorldMap = char [] []

let getWorldMap () : WorldMap =
    readLinesFromFile "Day03/input.txt" |> Array.map (fun line -> seq line |> Seq.toArray)

type Coordinate = { x: int; y: int }

let containsTree (worldMap: WorldMap, coordinate: Coordinate) =
    let row = worldMap.[coordinate.y]
    row.[coordinate.x % row.Length] = treeIndicator

type Trajectory = { right: int; down: int }

let partOne () =
    let trajectory = { right = 3; down = 1 }

    let worldMap = getWorldMap()
    let targetY = worldMap.Length

    let rec getTotalTreesHit (currentCoordinate: Coordinate, treesHitSoFar: int) =
        let newCoordinate = { x = currentCoordinate.x + trajectory.right; y = currentCoordinate.y + trajectory.down }

        if newCoordinate.y >= targetY then
            treesHitSoFar
        else
            getTotalTreesHit (newCoordinate, treesHitSoFar + (if containsTree (worldMap, newCoordinate) then 1 else 0))

    getTotalTreesHit ({ x = 0; y = 0 }, 0)

let partTwo () =
    // TODO
    0
