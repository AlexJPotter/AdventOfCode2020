module Day19_2

open FileHelpers

type RuleDefinition =
    | CharMatch of char
    | RulesMatch of int[]
    | AnyRulesMatch of int[][]

type Rule = {
    RuleNumber: int
    Definition: RuleDefinition
}

let rec printRuleDefinition (ruleDefinition: RuleDefinition) =
    match ruleDefinition with
    | CharMatch char -> $"'{char}'"
    | RulesMatch rules -> System.String.Join(" ", rules)
    | AnyRulesMatch rulesCollection ->
        System.String.Join(" | ", rulesCollection |> Array.map (fun rules -> RulesMatch rules) |> Array.map printRuleDefinition)

let printRule (rule: Rule) =
    $"{rule.RuleNumber}: {rule.Definition |> printRuleDefinition}"

let parseRuleLine (line: string) : Rule =
    let ruleNumber = line.Split(": ").[0] |> int

    let definition =
        if line.Contains "\"" then
            CharMatch (line.[(line.IndexOf '"') + 1])
        else
            AnyRulesMatch (line.Split(": ").[1].Split(" | ") |> Array.map (fun part -> part.Split(" ") |> Array.map int))

    { RuleNumber = ruleNumber; Definition = definition }

let input = readLinesFromFile "Day19/input_2.txt"

let allRules = input |> Array.filter (fun line -> line.Contains ":") |> Array.map parseRuleLine

let rulesByNumber = allRules |> Array.map (fun rule -> (rule.RuleNumber, rule)) |> Map.ofArray

let ruleZero = rulesByNumber.Item 0

let messages = input |> Array.filter (fun line -> line.Length > 0 && not (line.Contains ":"))

type CheckRuleResult = {
    Success: bool
    Remaining: string
}

let rec checkRule (rule: Rule) (message: string) : CheckRuleResult =
    if rule.RuleNumber = 8 || rule.RuleNumber = 11 then
        printfn $"Oops.. {rule.RuleNumber}"

    let ruleDefinition = rule.Definition

    match ruleDefinition with
    | CharMatch character ->
        let success = message.Length >= 1 && (message |> seq |> Seq.head) = character
        let remaining = if success && message.Length > 1 then message.Substring 1 else ""
        let result = { Success = success; Remaining = remaining; }
        result
    | RulesMatch rules ->
        if rules.Length = 1 then
            let rule = rulesByNumber.Item (rules |> Array.exactlyOne)
            checkRule rule message
        else
            let firstRule = rulesByNumber.Item (rules |> Array.head)
            let firstRuleResult = checkRule firstRule message
                
            if firstRuleResult.Success then
                let newMessage = firstRuleResult.Remaining
                let newRule = { RuleNumber = rule.RuleNumber; Definition = RulesMatch (rules |> Array.skip 1) }
                checkRule newRule newMessage
            else
                { Success = false; Remaining = "" }
    | AnyRulesMatch rulesCollection ->
        rulesCollection
        |> Array.map (fun rules -> { RuleNumber = rule.RuleNumber; Definition = RulesMatch rules })
        |> Array.fold (fun current nextRules -> if current.Success then current else (checkRule nextRules message)) ({ Success = false; Remaining = "" })

let solution () =
    let rule42 = rulesByNumber.Item(42)
    let checkRule42 = checkRule rule42

    let rule31 = rulesByNumber.Item(31)
    let checkRule31 = checkRule rule31

    // By observation, we see that valid messages for rule 8 are any
    // concatenation of messages which are valid according to rule 42.
    let rec isValidAccordingToRule8 (message: string) =
        let rule42Result = checkRule42 message

        if rule42Result.Success then
            rule42Result.Remaining = "" || isValidAccordingToRule8 rule42Result.Remaining
        else
            false

    // By observation, we see that valid messages for rule 11 are formed of
    // n messages which are valid according to rule 42 followed by n messages
    // which are valid according to rule 31.
    let rec isValidAccordingToRule11 (message: string) =
        let leadingSubstrings = [| 1..(message.Length - 1) |] |> Array.map (fun length -> message.Substring(0, length))

        let checkLeadingSubstring (leadingSubstring: string) =
            let remainingSubstring = message.Substring(leadingSubstring.Length)
            let trailingSubstrings = [| 1..(remainingSubstring.Length) |] |> Array.map (fun length -> remainingSubstring.Substring(0, length))
            
            let checkTrailingSubstring (trailingSubstring: string) =
                let leadingResult = checkRule42 leadingSubstring
                let trailingResult = checkRule31 trailingSubstring
                if leadingResult.Success && trailingResult.Success && trailingResult.Remaining = "" then
                    if leadingSubstring + trailingSubstring = message then
                        true
                    else
                        let lengthToRemoveFromStart = leadingSubstring.Length - leadingResult.Remaining.Length
                        let lengthToRemoveFromEnd = trailingSubstring.Length
                        let lengthOfRemainingMessage = message.Length - (lengthToRemoveFromStart + lengthToRemoveFromEnd)
                        let newMessage = message.Substring(lengthToRemoveFromStart, lengthOfRemainingMessage)
                        isValidAccordingToRule11 newMessage
                else
                    false

            trailingSubstrings |> Array.exists checkTrailingSubstring

        leadingSubstrings |> Array.exists checkLeadingSubstring

    // Using the above with the fact that rule 0 matches rule 8 followed by rule 11, we see that we need
    // to match rule 42 some number of times, and then rule 31 a strictly smaller number of times.
    let isValidAccordingToRule0 (message: string) =
        let rec reduceMessage (remaining: string) (applicationsOfRule42: int) =
            if applicationsOfRule42 < 2 then
                let result = checkRule42 remaining
                if result.Success && result.Remaining.Length > 1 then
                    reduceMessage (result.Remaining) (applicationsOfRule42 + 1)
                else
                    false
            else
                let rec reduceWithRule31 (remaining2: string) (applicationsRemaining: int) =
                    let result = checkRule31 remaining2
                    if not result.Success then
                        false
                    elif result.Remaining = "" then
                        true
                    elif (applicationsRemaining - 1) = 0 then
                        false
                    else
                        reduceWithRule31 (result.Remaining) (applicationsRemaining - 1)

                let restMatchesRule31 = reduceWithRule31 (remaining) (applicationsOfRule42 - 1)

                if restMatchesRule31 then
                    true
                else
                    let rule42Result = checkRule42 remaining
                    rule42Result.Success && rule42Result.Remaining.Length > 0 && (reduceMessage (rule42Result.Remaining) (applicationsOfRule42 + 1))

        reduceMessage (message) (0)

    messages |> Array.filter isValidAccordingToRule0 |> Array.length
