module Day01

open FileHelpers

exception FailedToGetSolutionError of string

let partOne () =
    let input = readLinesFromFile "Day01/input.txt"
    let numericInput = [ for i in input -> int i ]
    let sortedNumericInput = numericInput |> List.sort

    let mutable lowerIndex = 0
    let mutable upperIndex = (sortedNumericInput |> Seq.length) - 1

    let target = 2020

    let mutable foundSolution = false
    let mutable solution = 0

    while not foundSolution do
        let lowerValue = sortedNumericInput.Item(lowerIndex)
        let upperValue = sortedNumericInput.Item(upperIndex)
        let sum = lowerValue + upperValue

        if sum = target then
            foundSolution <- true
            solution <- (lowerValue * upperValue)
        
        elif sum < target
            then lowerIndex <- lowerIndex + 1

        elif sum > target
            then upperIndex <- upperIndex - 1

    solution

let partTwo () =
    let input = readLinesFromFile "Day01/input.txt"
    let numericInput = [ for i in input -> int i ]
    let sortedNumericInput = numericInput |> List.sort

    let mutable lowerIndex = 0
    let mutable middleIndex = 1
    let mutable upperIndex = (sortedNumericInput |> Seq.length) - 1

    let target = 2020

    let mutable foundSolution = false
    let mutable solution = 0

    while not foundSolution do
        let lowerValue = sortedNumericInput.Item(lowerIndex)
        let innerTarget = target - lowerValue

        middleIndex <- lowerIndex + 1
        upperIndex <- (sortedNumericInput |> Seq.length) - 1

        while (not foundSolution) && (middleIndex <> upperIndex) do
            let middleValue = sortedNumericInput.Item(middleIndex)
            let upperValue = sortedNumericInput.Item(upperIndex)
            let innerSum = middleValue + upperValue

            if innerSum = innerTarget then
                solution <- lowerValue * middleValue * upperValue
                foundSolution <- true

            elif innerSum < innerTarget then
                middleIndex <- middleIndex + 1

            elif innerSum > innerTarget then
                upperIndex <- upperIndex - 1

        lowerIndex <- lowerIndex + 1

    solution
