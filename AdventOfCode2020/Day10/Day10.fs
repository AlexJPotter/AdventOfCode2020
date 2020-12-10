module Day10

open FileHelpers

let getInput () =
    readLinesFromFile "Day10/input.txt" |> Array.map int

let partOne () =
    let rec solution (adapters: int [], oneJoltCount: int, threeJoltCount: int, currentJoltage: int) =
        let validAdapters = adapters |> Array.filter (fun a -> a > currentJoltage && a <= currentJoltage + 3)
        
        if validAdapters.Length = 0 then
            oneJoltCount * (threeJoltCount + 1)
        else
            let bestChoice = validAdapters |> Array.sort |> Array.head
            let difference = bestChoice - currentJoltage
            solution(
                adapters,
                oneJoltCount + (if difference = 1 then 1 else 0),
                threeJoltCount + (if difference = 3 then 1 else 0),
                bestChoice
            )

    solution (getInput(), 0, 0, 0)

let partTwo () =
    let input = getInput()

    let maxInput = input |> Array.max
    let endJoltage = maxInput + 3

    let getValidAdapters (fromJoltage: int) =
        let validAdapters = input |> Array.filter (fun a -> a > fromJoltage && a <= fromJoltage + 3)
        if validAdapters.Length > 0 then
            validAdapters
        elif fromJoltage = maxInput then
            [| endJoltage  |]
        else
            Array.empty

    let validAdaptersByJoltage =
        ([ for i in input -> i, getValidAdapters (i) ] |> Map).Add(0, getValidAdapters(0))

    let cachedPathCounts = Array.create<int64> ((input |> Array.max) + 1) -1L

    let rec getNumberOfPathsFrom (joltage: int) =
        if cachedPathCounts.[joltage] >= 0L then
            cachedPathCounts.[joltage]
        else
            let validAdapters = validAdaptersByJoltage.Item (joltage)

            if validAdapters.Length = 1 && validAdapters.[0] = endJoltage then
                cachedPathCounts.SetValue (1, joltage)
                1L
            elif validAdapters.Length = 0 then
                cachedPathCounts.SetValue (0, joltage)
                0L
            else
                let result = validAdapters |> Array.map getNumberOfPathsFrom |> Array.sum
                cachedPathCounts.SetValue (result, joltage)
                result

    getNumberOfPathsFrom 0
