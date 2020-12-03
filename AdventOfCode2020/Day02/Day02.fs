module Day02

open FileHelpers

let partOne () =
    let input = readLinesFromFile "Day02/input.txt"

    let mutable numberOfValidPasswords = 0

    for line in input do
        let split = line.Split(" ")

        let minFrequency = split.[0].Split("-").[0] |> int
        let maxFrequency = split.[0].Split("-").[1] |> int

        let character = split.[1].Substring(0, 1).ToCharArray().[0]

        let password = split.[2]

        let mutable characterCount = 0

        for c in password do
            characterCount <- characterCount + (if c = character then 1 else 0)

        let isValidPassword = characterCount >= minFrequency && characterCount <= maxFrequency
        numberOfValidPasswords <- numberOfValidPasswords + (if isValidPassword then 1 else 0)

    numberOfValidPasswords

let partTwo () =
    let input = readLinesFromFile "Day02/input.txt"

    let mutable numberOfValidPasswords = 0

    for line in input do
        let split = line.Split(" ")

        let positionA = split.[0].Split("-").[0] |> int
        let positionB = split.[0].Split("-").[1] |> int

        let character = split.[1].Substring(0, 1).ToCharArray().[0]

        let password = split.[2]

        let matchInPositionA = (password.[positionA - 1] = character)
        let matchInPositionB = (password.[positionB - 1] = character)

        let isValidPassword = (matchInPositionA && not matchInPositionB) || (matchInPositionB && not matchInPositionA)

        numberOfValidPasswords <- numberOfValidPasswords + (if isValidPassword then 1 else 0)

    numberOfValidPasswords
