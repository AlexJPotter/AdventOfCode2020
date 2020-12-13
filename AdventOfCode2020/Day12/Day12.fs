module Day12

open System
open FileHelpers

let input = readLinesFromFile "Day12/input.txt"

let directions = [| 'N'; 'E'; 'S'; 'W' |]

let turnRight (degrees: int, currentDirection: char) =
    let numberOfTurns = degrees / 90
    let currentDirectionIndex = directions |> Array.findIndex (fun d -> d = currentDirection)
    directions.[(currentDirectionIndex + numberOfTurns) % directions.Length]

let turnLeft (degrees: int, currentDirection: char) =
    let numberOfTurns = degrees / 90
    let currentDirectionIndex = directions |> Array.findIndex (fun d -> d = currentDirection)
    directions.[(currentDirectionIndex + directions.Length - numberOfTurns) % directions.Length]

let getVector (currentDirection: char) =
    match currentDirection with
    | 'N' -> (0, 1)
    | 'E' -> (1, 0)
    | 'S' -> (0, -1)
    | 'W' -> (-1, 0)
    | _ -> (0, 0)

let parseInstruction (instruction: string) =
    (instruction |> seq |> Seq.head, instruction |> seq |> Seq.skip(1) |> Seq.toArray |> String |> int)

let getNewPosition (currentPosition: int * int, vector: int * int, magnitude: int) =
    let (xPosition, yPosition) = currentPosition
    let (xVector, yVector) = vector
    (xPosition + (xVector * magnitude), yPosition + (yVector * magnitude))

let partOne () =
    let rec processInstructions (instructions: string [], currentDirection: char, currentPosition: int * int) =
        if instructions.Length = 0 then
            let (x, y) = currentPosition
            (x |> Math.Abs) + (y |> Math.Abs)
        else
            let (action, magnitude) = instructions |> Array.head |> parseInstruction
            let remainingInstructions = instructions |> Array.skip(1)
            match action with
            | 'L' -> processInstructions (remainingInstructions, turnLeft(magnitude, currentDirection), currentPosition)
            | 'R' -> processInstructions (remainingInstructions, turnRight(magnitude, currentDirection), currentPosition)
            | 'F' -> processInstructions (remainingInstructions, currentDirection, getNewPosition(currentPosition, getVector(currentDirection), magnitude))
            | direction -> processInstructions (remainingInstructions, currentDirection, getNewPosition (currentPosition, getVector(direction), magnitude))

    processInstructions(input, 'E', (0, 0))

let partTwo () =
    let rec rotateRight (vector: int * int, times: int) =
        let (x, y) = vector
        let rotated = (y, -1 * x)
        if times = 1 then
            rotated
        else
            rotateRight(rotated, times - 1)

    let rec rotateLeft (vector: int * int, times: int) =
        let (x, y) = vector
        let rotated = (-1 * y, x)
        if times = 1 then
            rotated
        else
            rotateLeft(rotated, times - 1)

    let rec processInstructions (instructions: string [], currentPosition: int * int, currentWaypoint: int * int) =
        if instructions.Length = 0 then
            let (x, y) = currentPosition
            (x |> Math.Abs) + (y |> Math.Abs)
        else
            let (action, magnitude) = instructions |> Array.head |> parseInstruction
            let remainingInstructions = instructions |> Array.skip(1)
            match action with
            | 'L' -> processInstructions (remainingInstructions, currentPosition, rotateLeft (currentWaypoint, magnitude / 90))
            | 'R' -> processInstructions (remainingInstructions, currentPosition, rotateRight (currentWaypoint, magnitude / 90))
            | 'F' -> processInstructions (remainingInstructions, getNewPosition (currentPosition, currentWaypoint, magnitude), currentWaypoint)
            | direction -> processInstructions (remainingInstructions, currentPosition, getNewPosition (currentWaypoint, getVector(direction), magnitude))

    processInstructions (input, (0, 0), (10, 1))
