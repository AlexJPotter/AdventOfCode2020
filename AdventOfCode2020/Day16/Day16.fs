module Day16

open FileHelpers

type Rule = { field: string; range1: int * int; range2: int * int }

type Ticket = int[]

let parseRule (line: string) : Rule =
    let parts = line.Split(": ")
    let field = parts.[0]
    let ranges = parts.[1].Split(" or ")
    let range1 = (ranges.[0].Split("-").[0] |> int, ranges.[0].Split("-").[1] |> int)
    let range2 = (ranges.[1].Split("-").[0] |> int, ranges.[1].Split("-").[1] |> int)
    { field = field; range1 = range1; range2 = range2 }

let parseTicket (line: string) : Ticket =
    line.Split(",") |> Array.map int

let input = readLinesFromFile "Day16/input.txt"

let rules = input |> Array.filter (fun line -> line.Contains(" or ")) |> Array.map parseRule

let allTickets = input |> Array.filter (fun line -> line.Contains(",")) |> Array.map parseTicket

let yourTicket = allTickets |> Array.head
let nearbyTickets = allTickets |> Array.skip 1

let isValidAccordingToRule (value: int) (rule: Rule) =
    let (lower1, upper1) = rule.range1
    let (lower2, upper2) = rule.range2
    (value >= lower1 && value <= upper1) || (value >= lower2 && value <= upper2)

let isInvalidAccordingToRule (value: int) (rule: Rule) =
    not (isValidAccordingToRule value rule)

let isInvalidAccordingToAllRules (value: int) =
    let checkRule = isInvalidAccordingToRule value
    rules |> Array.forall checkRule

let getInvalidValues (ticket: Ticket) =
    ticket |> Array.filter isInvalidAccordingToAllRules

let getSumOfInvalidValues (ticket: Ticket) =
    ticket |> getInvalidValues |> Array.sum

let mightBeValid (ticket: Ticket) =
    ticket |> getInvalidValues |> Array.isEmpty

let partOne () =
    nearbyTickets |> Array.sumBy getSumOfInvalidValues

let partTwo () =
    let tickets = allTickets |> Array.filter mightBeValid

    let isValidForPositionInTicket (rule: Rule) (position: int) (ticket: Ticket) =
        let value = ticket.[position]
        isValidAccordingToRule value rule

    let isValidForPositionInAllTickets (rule: Rule) (position: int) =
        let isValid = isValidForPositionInTicket rule position
        tickets |> Array.forall isValid

    let ticketLength = tickets |> Array.head |> Array.length

    let positions = [|0..(ticketLength - 1)|]

    let getPositionsInWhichRuleIsValid (rule: Rule) =
        let isValid = isValidForPositionInAllTickets rule
        positions |> Array.filter isValid |> Set.ofArray

    let fieldsWithValidPositions =
        rules
        |> Array.map (fun rule -> (rule.field, getPositionsInWhichRuleIsValid rule))
        |> Array.sortBy (fun (_, validPositions) -> validPositions |> Set.toArray |> Array.length)

    let withoutPosition (position: int) (fieldAndValidPositions: string * Set<int>) =
        let (field, validPositions) = fieldAndValidPositions
        (field, validPositions.Remove(position))

    let withPositionChosen (position: int) (fieldsAndValidPositions: (string * Set<int>)[]) =
        fieldsAndValidPositions |> Array.skip 1 |> Array.map (withoutPosition position)

    let rec hasSomeValidConfiguration (fieldsAndValidPositions: (string * Set<int>)[]) =
        if fieldsAndValidPositions.Length = 0 then
            false
        elif fieldsAndValidPositions.Length = 1 then
            let (_, validPositions) = fieldsAndValidPositions |> Array.exactlyOne
            not validPositions.IsEmpty
        else
            let (_, validPositions) = fieldsAndValidPositions |> Array.head
            validPositions
            |> Set.toArray
            |> Array.map (fun position -> fieldsAndValidPositions |> withPositionChosen position)
            |> Array.exists hasSomeValidConfiguration

    let choosePosition (fieldsAndValidPositions: (string * Set<int>)[]) =
        let (field, validPositions) = fieldsAndValidPositions |> Array.head

        if fieldsAndValidPositions.Length = 1 then
            (field, validPositions |> Set.toArray |> Array.head)
        else
            let positionCanBeChosen (position: int) =
                hasSomeValidConfiguration (withPositionChosen (position) (fieldsAndValidPositions))

            let chosenPosition = validPositions |> Set.toArray |> Array.find positionCanBeChosen

            (field, chosenPosition)

    let rec solve (fieldsAndValidPositions: (string * Set<int>)[]) (chosenPositions: List<string * int>) =
        if fieldsAndValidPositions.Length = 0
            then chosenPositions
        else
            let fieldAndChosenPosition = choosePosition fieldsAndValidPositions
            let (_, chosenPosition) = fieldAndChosenPosition
            let newChosenPositions = chosenPositions @ [ fieldAndChosenPosition ]
            solve (withPositionChosen (chosenPosition) (fieldsAndValidPositions)) (newChosenPositions)

    let chosenPositions = solve (fieldsWithValidPositions) (List.empty)

    let departurePositions =
        chosenPositions
        |> List.filter (fun (field, _) -> field.StartsWith("departure"))
        |> List.map (fun (_, position) -> position)
        |> Set.ofList

    let departureValues =
        yourTicket
        |> Array.mapi (fun index value -> (value, index))
        |> Array.filter (fun (_, index) -> departurePositions.Contains(index))
        |> Array.map (fun (value, _) -> value)

    departureValues |> Array.map int64 |> Array.reduce (fun x y -> x * y)
