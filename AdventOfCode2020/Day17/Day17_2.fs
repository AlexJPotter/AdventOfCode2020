﻿module Day17_2

type Coordinate = int * int * int * int

open FileHelpers

let readInputLine (y: int) (line: string) : Coordinate[] =
    line
    |> seq
    |> Seq.mapi (fun x element -> (element, x))
    |> Seq.filter (fun (element, _) -> element = '#')
    |> Seq.map (fun (element, x) -> (x, y, 0, 0))
    |> Seq.toArray

let initiallyActiveCoordinates =
    readLinesFromFile "Day17/input.txt"
    |> Array.mapi readInputLine
    |> Array.collect id
    |> Array.distinct
    |> Set.ofArray

let getNeighbours (coordinate: Coordinate) : Coordinate[] =
    let offsets = [| -1; 0; 1 |]
    let xyPairings = Array.allPairs offsets offsets
    let xyzPairings = Array.allPairs xyPairings offsets |> Array.map (fun ((x, y), z) -> (x, y, z))
    let xyzwPairings = Array.allPairs xyzPairings offsets |> Array.map (fun ((x, y, z), w) -> (x, y, z, w))
    let coordinateOffsets = xyzwPairings |> Array.filter (fun (x, y, z, w) -> x <> 0 || y <> 0 || z <> 0 || w <> 0)

    let (x, y, z, w) = coordinate

    coordinateOffsets |> Array.map (fun (a, b, c, d) -> (x + a, y + b, z + c, w + d))

let isActiveNext (activeCoordinates: Set<Coordinate>) (coordinate: Coordinate) =
    let isActive = activeCoordinates.Contains coordinate
    let activeNeighbours = getNeighbours (coordinate) |> Array.filter activeCoordinates.Contains |> Array.length
    if isActive then
        activeNeighbours = 2 || activeNeighbours = 3
    else
        activeNeighbours = 3

let solution () =
    let rec executeCycle (times: int) (activeCoordinates: Set<Coordinate>) =
        let coordinatesToConsider = activeCoordinates |> Set.toArray |> Array.map getNeighbours |> Array.collect id |> Array.distinct
        let nextActiveCoordinates = coordinatesToConsider |> Array.filter (isActiveNext (activeCoordinates)) |> Set.ofArray
        if times - 1 = 0 then
            nextActiveCoordinates
        else
            executeCycle (times - 1) (nextActiveCoordinates)

    let finalActiveCoordinates = executeCycle (6) (initiallyActiveCoordinates)

    finalActiveCoordinates |> Set.count
