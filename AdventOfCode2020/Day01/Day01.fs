module Day01

open FileHelpers

let testInput = seq { 299; 366; 675; 979; 1456; 1721 }

let getInput () =
    readLinesFromFile "Day01/input.txt" |> Seq.map int |> Seq.sort

let withoutFirstElement<'T> (sequence: seq<'T>) =
    let lengthOfSequence = sequence |> Seq.length
    sequence |> Seq.skip 1 |> Seq.take (lengthOfSequence - 1)

let withoutLastElement<'T> (sequence: seq<'T>) =
    let lengthOfSequence = sequence |> Seq.length
    sequence |> Seq.take (lengthOfSequence - 1)

let areOfEqualLength<'T> (sequenceA: seq<'T>, sequenceB: seq<'T>) =
    Seq.length (sequenceA) = Seq.length (sequenceB)

type FindSolutionResult = { success: bool; solution: int }

let rec findSolution (targetValue: int, sequence: seq<int>) =
    let firstElement = sequence |> Seq.head
    let lastElement = sequence |> Seq.last

    let sum = firstElement + lastElement
    let differenceFromTarget = targetValue - sum

    let reducedSequence =
        match differenceFromTarget with
        | diff when diff < 0 -> sequence |> withoutLastElement
        | diff when diff > 0 -> sequence |> withoutFirstElement
        | _ -> sequence

    if areOfEqualLength (sequence, reducedSequence) then
        { success = true; solution = (firstElement * lastElement) }
    elif Seq.length (reducedSequence) = 1 then
        { success = false; solution = 0 }
    else
        findSolution (targetValue, reducedSequence)

let partOne () =
    let input = getInput()
    (findSolution (2020, input)).solution

let partTwo () =
    let input = getInput()

    let rec findSolution2 sequence =
        let firstElement = sequence |> Seq.head
        let restOfSequence = sequence |> withoutFirstElement

        let target = 2020 - firstElement

        let solutionResult = findSolution (target, restOfSequence)

        if solutionResult.success then
            { success = true; solution = firstElement * solutionResult.solution }
        elif Seq.length restOfSequence = 2 then
            { success = false; solution = 0 }
        else restOfSequence |> findSolution2

    (input |> Seq.sort |> findSolution2).solution
