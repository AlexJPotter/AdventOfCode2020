module Day15

let input = [| 18L; 11L; 9L; 0L; 5L; 1L |]

let rec solve (numbersSeenBeforeLastNumber: Set<int64>, latestNumberPositions: Map<int64, (int64 * int64)>, lastNumber: int64, turnNumber: int64) (endTurn: int64) =
    let numberToSpeak =
        if numbersSeenBeforeLastNumber.Contains(lastNumber) then
            let (beforeThen, previouslySpoken) = latestNumberPositions.Item(lastNumber)
            previouslySpoken - beforeThen
        else
            0L

    if turnNumber = endTurn then
        numberToSpeak
    else
        let (_, previouslySpoken) = if latestNumberPositions.ContainsKey(numberToSpeak) then latestNumberPositions.Item(numberToSpeak) else (turnNumber, turnNumber)
        let newLatestNumberPositions = latestNumberPositions.Add(numberToSpeak, (previouslySpoken, turnNumber))
        solve (numbersSeenBeforeLastNumber.Add(lastNumber), newLatestNumberPositions, numberToSpeak, turnNumber + 1L) (endTurn) 

let initialNumbersSeenBeforeLastNumber = input |> Array.take(input.Length - 1) |> Set.ofArray

let initialLatestNumberPositions = input |> Array.mapi (fun index number -> (number, (int64 index + 1L, int64 index + 1L))) |> Map.ofArray

let solver = solve (initialNumbersSeenBeforeLastNumber, initialLatestNumberPositions, input |> Array.last, int64 (input |> Array.length) + 1L)

let partOne () =
    solver 2020L

let partTwo () =
    solver 30_000_000L
