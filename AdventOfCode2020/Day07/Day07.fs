module Day07

open FileHelpers

type ColourAndChildColours = { Colour: string; ChildColours: string [] }

let partOne () =
    let parseLine (line: string) = 
        let parts = line.Split(" bags contain ")
        let colour = parts.[0]
        let ruleString = parts.[1]
        let trimmedRuleString = ruleString.Substring(0, ruleString.Length - 1)

        let childColours =
            (if trimmedRuleString = "no other bags" then [||] else trimmedRuleString.Split(", "))
            |> Array.map (fun r -> r.Split(" bag").[0])
            |> Array.map (fun r -> r.Substring(r.Split(" ").[0].Length + 1))

        { Colour = colour; ChildColours = childColours }

    let input =
        readLinesFromFile("Day07/input.txt")
        |> Array.filter (fun l -> l.Length > 0)
        |> Array.map parseLine
    
    let rec doesColourHoldGoldBag (colour: string) =
        let bagDetails = input |> Array.find (fun x -> x.Colour = colour)
        let childColours = bagDetails.ChildColours
        
        childColours.Length > 0 &&
        (childColours |> Array.contains("shiny gold") || childColours |> Array.exists doesColourHoldGoldBag)

    input |> Array.map (fun l -> l.Colour) |> Array.filter doesColourHoldGoldBag |> Array.length

type BagQuantity = { Colour: string; Quantity: int }

let partTwo () = 
    let parseLine (line: string) = 
        let parts = line.Split(" bags contain ")
        let colour = parts.[0]
        let ruleString = parts.[1]
        let trimmedRuleString = ruleString.Substring(0, ruleString.Length - 1)

        let getBagQuantityFromRuleString (r : string) =
            let qty = r.Split(" ").[0] |> int
            let col = r.Substring(qty.ToString().Length + 1)
            { Colour = col; Quantity = qty }

        let childColours =
            (if trimmedRuleString = "no other bags" then [||] else trimmedRuleString.Split(", "))
            |> Array.map (fun r -> r.Split(" bag").[0])
            |> Array.map getBagQuantityFromRuleString

        (colour, childColours)

    let childBagQuantitiesByColour =
        readLinesFromFile("Day07/input.txt")
        |> Array.filter (fun l -> l.Length > 0)
        |> Array.map parseLine
        |> Map.ofArray

    let rec countContainedBags (colour: string) =
        let quantities = childBagQuantitiesByColour.Item(colour)
        
        if (quantities.Length = 0) then
            0
        else
            (quantities |> Array.map (fun q -> q.Quantity) |> Array.sum) +
            (quantities |> Array.map (fun q -> q.Quantity * countContainedBags(q.Colour)) |> Array.sum)

    countContainedBags("shiny gold")
