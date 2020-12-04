module Day02

open FileHelpers

type PasswordLine = { minFrequency: int; maxFrequency: int; targetCharacter: char; password: string }

let parseInputLine (inputLine: string) =
    let split = inputLine.Split(" ")
    let minFrequency = split.[0].Split("-").[0] |> int
    let maxFrequency = split.[0].Split("-").[1] |> int
    let targetCharacter = split.[1].Substring(0, 1).ToCharArray().[0]
    let password = split.[2]
    { minFrequency=minFrequency; maxFrequency=maxFrequency; targetCharacter=targetCharacter; password=password; }

let partOne () =
    let isValidPassword (password: string, targetCharacter: char, minFrequency: int, maxFrequency: int) =
        let characterFrequency = seq password |> Seq.filter (fun c -> c = targetCharacter) |> Seq.length
        (characterFrequency >= minFrequency) && (characterFrequency <= maxFrequency)

    let isValidPasswordLine (passwordLine: PasswordLine) =
        isValidPassword(passwordLine.password, passwordLine.targetCharacter, passwordLine.minFrequency, passwordLine.maxFrequency)

    let input = readLinesFromFile "Day02/input.txt"

    seq input |> Seq.map parseInputLine |> Seq.filter isValidPasswordLine |> Seq.length

let partTwo () =
    let isValidPassword (password: string, targetCharacter: char, indexA: int, indexB: int) =
        let matchAtIndex (index: int) =
            password.[index - 1] = targetCharacter

        let xor (a: bool, b: bool) =
            a && not b || b && not a

        xor (matchAtIndex indexA, matchAtIndex indexB)

    let isValidPasswordLine (passwordLine: PasswordLine) =
        isValidPassword(passwordLine.password, passwordLine.targetCharacter, passwordLine.minFrequency, passwordLine.maxFrequency)

    let input = readLinesFromFile "Day02/input.txt"

    seq input |> Seq.map parseInputLine |> Seq.filter isValidPasswordLine |> Seq.length
