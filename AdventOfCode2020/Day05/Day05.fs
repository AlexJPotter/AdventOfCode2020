module Day05

open FileHelpers

let getInput () =
    readLinesFromFile "Day05/input.txt"

type Range = { lower: int; upper: int }

let getRangeSize (range: Range) =
    (range.upper - range.lower) + 1

let getUpperHalfOfRange (range: Range) =
    { lower = range.lower + (getRangeSize(range) / 2); upper = range.upper }

let getLowerHalfOfRange (range: Range) =
    { lower = range.lower; upper = range.upper - (getRangeSize(range) / 2) }

let getSeatId (boardingPass: string) =
    let rec reduceRange (rowRange: Range, seatRange: Range, charactersLeftToProcess: List<char>) =
        // printfn "%d/%d\t%d/%d\t%A" rowRange.lower rowRange.upper seatRange.lower seatRange.upper charactersLeftToProcess
        match charactersLeftToProcess with
        | head :: tail when head = 'F' -> reduceRange (getLowerHalfOfRange(rowRange), seatRange, tail)
        | head :: tail when head = 'B' -> reduceRange (getUpperHalfOfRange(rowRange), seatRange, tail)
        | head :: tail when head = 'L' -> reduceRange (rowRange, getLowerHalfOfRange(seatRange), tail)
        | head :: tail when head = 'R' -> reduceRange (rowRange, getUpperHalfOfRange(seatRange), tail)
        | _ -> (rowRange.lower * 8) + seatRange.lower

    reduceRange ({ lower = 0; upper = 127 }, { lower = 0; upper = 7 }, seq boardingPass |> Seq.toList)

let testInput = [| "FBFBBFFRLR" |]

let partOne () =
    getInput() |> Array.map getSeatId |> Array.max

let partTwo () =
    let rec findSeatId (seatIdsToCheck: List<int>) =
        match seatIdsToCheck with
        | head :: tail when (tail |> List.head) = head + 1 -> tail |> findSeatId
        | _ -> (seatIdsToCheck |> List.head) + 1

    getInput() |> Array.map getSeatId |> Array.sort |> Array.toList |> findSeatId
