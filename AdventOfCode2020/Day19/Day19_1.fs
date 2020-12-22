module Day19_1

open FileHelpers

type Rule =
    | CharMatch of char
    | RulesMatch of int[]
    | AnyRulesMatch of int[][]

let rec printRule (rule: Rule) =
    match rule with
    | CharMatch char -> $"{char}"
    | RulesMatch rules -> System.String.Join(" ", rules)
    | AnyRulesMatch rulesCollection ->
        System.String.Join(" | ", rulesCollection |> Array.map (fun rules -> RulesMatch rules) |> Array.map printRule)

let parseRuleLine (line: string) =
    let ruleNumber = line.Split(": ").[0] |> int

    if line.Contains "\"" then
        (ruleNumber, CharMatch (line.[(line.IndexOf '"') + 1]))
    else
        (ruleNumber, AnyRulesMatch (line.Split(": ").[1].Split(" | ") |> Array.map (fun part -> part.Split(" ") |> Array.map int)))

let testInputText = "0: 4 1 5
1: 2 3 | 3 2
2: 4 4 | 5 5
3: 4 5 | 5 4
4: \"a\"
5: \"b\"

ababbb
bababa
abbbab
aaabbb
aaaabbb"

let testInput = testInputText.Replace("\r\n", "\n").Split("\n")

let input = readLinesFromFile "Day19/input.txt"

let rulesByNumber = input |> Array.filter (fun line -> line.Contains ":") |> Array.map parseRuleLine |> Map.ofArray
let ruleZero = rulesByNumber.Item 0

let messages = input |> Array.filter (fun line -> line.Length > 0 && not (line.Contains ":"))

type CheckRuleResult = {
    Success: bool
    Remaining: string
}

let rec checkRule (rule: Rule) (message: string) : CheckRuleResult =
    let result =
        match rule with
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
                    let newRule = RulesMatch (rules |> Array.skip 1)
                    checkRule newRule newMessage
                else
                    { Success = false; Remaining = "" }
        | AnyRulesMatch rulesCollection ->
            rulesCollection
            |> Array.map (fun rules -> RulesMatch rules)
            |> Array.fold (fun current next -> if current.Success then current else (checkRule next message)) ({ Success = false; Remaining = "" })

    result

let solution () =
    messages
    |> Array.map (fun message -> (checkRule ruleZero message))
    |> Array.filter (fun result -> result.Success && result.Remaining = "")
    |> Array.length
