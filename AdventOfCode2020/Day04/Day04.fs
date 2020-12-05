module Day04

open System
open FileHelpers

let birthYear = "byr"
let issueYear = "iyr"
let expirationYear = "eyr"
let height = "hgt"
let hairColour = "hcl"
let eyeColour = "ecl"
let passportId = "pid"
let countryId = "cid"

let requiredFields = [| birthYear; issueYear; expirationYear; height; hairColour; eyeColour; passportId |]

let hasRequiredFields (passportString: string) =
    requiredFields
        |> Array.map (fun field -> passportString.Contains(field + ":"))
        |> Array.reduce (fun a b -> a && b)

type PassportField = { key: string; value: string }

let isNumeric (value: string) =
    seq value |> Seq.forall Char.IsDigit

let isBetween (value: int, lowerBound: int, upperBound: int) =
    value >= lowerBound && value <= upperBound 

let validateBirthYear (value: string) =
    isNumeric (value) && isBetween (int (value), 1920, 2002)
    
let validateIssueYear (value: string) =
    isNumeric (value) && isBetween (int (value), 2010, 2020)

let validateExpirationYear (value: string) =
    isNumeric (value) && isBetween (int (value), 2020, 2030)

let validateHeight (value: string) =
    let getValueWithoutUnit () =
        value.Substring (0, value.Length - 2)

    if value.EndsWith "cm" then
        let valueWithoutUnit = getValueWithoutUnit()
        isNumeric (valueWithoutUnit) && isBetween (int (valueWithoutUnit), 150, 193)
    elif value.EndsWith "in" then
        let valueWithoutUnit = getValueWithoutUnit()
        isNumeric (valueWithoutUnit) && isBetween (int (valueWithoutUnit), 59, 76)
    else
        false

let validateHairColour (value: string) =
    let validCharacters = seq "abcdef0123456789" |> Seq.toArray

    let getValueWithoutHash () =
        value.Substring(1)

    let isValidCharacter (character: char) =
        validCharacters |> Array.contains character

    let isValidColour () =
        let valueWithoutHash = getValueWithoutHash()
        seq valueWithoutHash |> Seq.forall isValidCharacter

    value.StartsWith("#") && isValidColour()

let validateEyeColour (value: string) =
    let validValues = [| "amb"; "blu"; "brn"; "gry"; "grn"; "hzl"; "oth" |]
    validValues |> Array.contains value

let validatePassportId (value: string) =
    value.Length = 9 && value |> isNumeric

let validateField (field: PassportField) =
    field.value |>
        match field.key with
        | "byr" -> validateBirthYear
        | "iyr" -> validateIssueYear
        | "eyr" -> validateExpirationYear
        | "hgt" -> validateHeight
        | "hcl" -> validateHairColour
        | "ecl" -> validateEyeColour
        | "pid" -> validatePassportId
        | "cid" -> (fun _ -> true)
        | _ -> (fun _ -> false)

let parseFieldString (fieldString: string) =
    let parts = fieldString.Split(":")
    if parts.Length <> 2 then printfn "Parsing field string '%s'" fieldString
    { key = parts.[0]; value = parts.[1] }

let hasValidFields (passportString: string) =
    passportString.Replace("\n", " ").Split(" ")
        |> Array.filter (fun part -> part <> "")
        |> Array.map parseFieldString
        |> Array.map validateField
        |> Array.reduce (fun a b -> a && b)

let getPassportStrings () =
    (readTextFromFile "Day04/input.txt").Replace("\r\n", "\n").Split("\n\n")

let partOne () =
    getPassportStrings() |> Array.filter hasRequiredFields |> Array.length

let partTwo () =
    let hasRequiredAndValidFields (passportString: string) =
        hasRequiredFields(passportString) && hasValidFields(passportString)

    getPassportStrings() |> Array.filter hasRequiredAndValidFields |> Array.length
