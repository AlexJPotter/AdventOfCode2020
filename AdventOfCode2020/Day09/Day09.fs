module Day09

open System.Numerics
open FileHelpers

let getInput () =
    readLinesFromFile "Day09/input.txt" |> Array.map BigInteger.Parse

let withoutFirstElement<'T> (sequence: 'T []) =
    let lengthOfSequence = sequence |> Array.length
    sequence |> Array.skip 1 |> Array.take (lengthOfSequence - 1)

let withoutLastElement<'T> (sequence: 'T []) =
    let lengthOfSequence = sequence |> Array.length
    sequence |> Array.take (lengthOfSequence - 1)

let areOfEqualLength<'T> (sequenceA: 'T [], sequenceB: 'T []) =
    Array.length (sequenceA) = Array.length (sequenceB)

let rec hasPairWhichSumToTarget (targetValue: bigint, sequence: bigint []) =
    let firstElement = sequence |> Array.head
    let lastElement = sequence |> Array.last

    let sum = firstElement + lastElement
    let differenceFromTarget = targetValue - sum

    let reducedSequence =
        match differenceFromTarget with
        | diff when diff < bigint(0) -> sequence |> withoutLastElement
        | diff when diff > bigint(0) -> sequence |> withoutFirstElement
        | _ -> sequence

    if areOfEqualLength (sequence, reducedSequence) then
        true
    elif Array.length (reducedSequence) = 1 then
        false
    else
        hasPairWhichSumToTarget (targetValue, reducedSequence)

let partOne () =
    let input = getInput()

    let rec getSolution (sequence: bigint []) =
        let number = sequence.[25]
        let precedingNumbers = sequence |> Array.take 25

        if hasPairWhichSumToTarget (number, precedingNumbers |> Array.sort) then
            getSolution (sequence |> withoutFirstElement)
        else
            number

    getSolution (input)

let partTwo () =
    let target = partOne()
    let input = getInput()

    let rec findRange (size: int, index: int) =
        if index + size > input.Length then
            findRange (size + 1, 0)
        else
            let range = input |> Array.skip (index) |> Array.take (size)
            let sum = range |> Array.sum
            if sum = target then
                range
            else
                findRange (size, index + 1)

    let solutionRange = findRange (2, 0)

    (solutionRange |> Array.min) + (solutionRange |> Array.max)
