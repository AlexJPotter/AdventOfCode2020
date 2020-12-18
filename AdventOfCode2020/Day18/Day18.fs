module Day18

open FileHelpers

type Operator =
    | AdditionOperator
    | MultiplicationOperator

type Element =
    | Number of int64
    | Operator of Operator
    | OpeningParenthesis
    | ClosingParenthesis

type Expression = Element[]

let printElement (element: Element) : string =
    match element with
    | Number number -> $"{number}"
    | Operator AdditionOperator -> " + "
    | Operator MultiplicationOperator -> " * "
    | OpeningParenthesis -> "("
    | ClosingParenthesis -> ")"

let printExpression (expression: Expression) : string =
    expression |> Array.map printElement |> Array.fold (fun x y -> x + y) ("")

let testInput = [| "1 + (2 * 3) + (4 * (5 + 6))" |]

let rawInput = readLinesFromFile "Day18/input.txt"

let parseCharacter (character: char) : Element =
    match character with
    | '+' -> Operator AdditionOperator
    | '*' -> Operator MultiplicationOperator
    | '(' -> OpeningParenthesis
    | ')' -> ClosingParenthesis
    | character -> Number (System.Char.GetNumericValue(character) |> int64)

let input : Expression[] =
    rawInput
    |> Array.map (fun line -> line.Replace(" ", ""))
    |> Array.map (fun line -> line |> seq |> Seq.map parseCharacter |> Seq.toArray)

// Returns a nested expression without the opening/closing parentheses
let getInnerExpression (expression: Expression) (openingParenthesisIndex: int) : Expression =
    let rec buildExpression (currentIndex: int) (nesting: int) (expressionSoFar: Expression) =
        if nesting = 0 then
            expressionSoFar
        else
            let element = expression.[currentIndex]
            let newExpression : Expression = Array.append expressionSoFar [| element |]
            match element with
            | OpeningParenthesis -> buildExpression (currentIndex + 1) (nesting + 1) (newExpression)
            | ClosingParenthesis -> if nesting = 1 then expressionSoFar else buildExpression (currentIndex + 1) (nesting - 1) (newExpression)
            | _ -> buildExpression (currentIndex + 1) (nesting) (newExpression)

    buildExpression (openingParenthesisIndex + 1) (1) (Array.empty)

let rec evaluate (expression: Expression) : int64 =
    let (left, middle, right) = (expression.[0], expression.[1], expression.[2])

    let (result, numberOfElementsReduced) =
        match (left, middle, right) with
        | (Number numLeft, Operator AdditionOperator, Number numRight) ->
            (numLeft + numRight, 3)
        | (Number numLeft, Operator MultiplicationOperator, Number numRight) ->
            (numLeft * numRight, 3)
        | (OpeningParenthesis, _, _) ->
            let innerExpression = getInnerExpression expression 0
            (evaluate innerExpression, innerExpression.Length + 2)
        | (Number numLeft, Operator AdditionOperator, OpeningParenthesis) ->
            let innerExpression = getInnerExpression expression 2
            (numLeft + (evaluate innerExpression), 2 + innerExpression.Length + 2) // Left, middle, inner expression, parentheses
        | (Number numLeft, Operator MultiplicationOperator, OpeningParenthesis) ->
            let innerExpression = getInnerExpression expression 2
            (numLeft * (evaluate innerExpression), 2 + innerExpression.Length + 2) // Left, middle, inner expression, parentheses
        | _ -> failwithf $"Could not evaluate {left} {middle} {right}"

    let remainingElements = expression |> Array.skip numberOfElementsReduced
    let newExpression = Array.append ([| Number result |]) (remainingElements)

    if newExpression.Length = 1 then
        result
    else
        evaluate newExpression

let partOne () =
    input |> Array.map evaluate |> Array.sum

let rec evaluate2 (expression: Expression) : int64 =
    if expression.Length = 1 then
        match expression.[0] with
        | Number number -> number
        | _ -> failwithf $"Could not evaluate {printExpression expression}"
    else
        let indexOfFirstOpeningParenthesis =
            expression
            |> Array.tryFindIndex (fun element ->
                match element with
                | OpeningParenthesis -> true
                | _ -> false
            )

        let indexOfFirstAdditionOperator =
            expression
            |> Array.tryFindIndex (fun element ->
                match element with
                | Operator AdditionOperator -> true
                | _ -> false
            )

        let indexToConsider =
            if indexOfFirstOpeningParenthesis.IsSome then
                indexOfFirstOpeningParenthesis.Value
            elif indexOfFirstAdditionOperator.IsSome then
                indexOfFirstAdditionOperator.Value - 1
            else
                0

        let (left, middle, right) = (expression.[indexToConsider], expression.[indexToConsider + 1], expression.[indexToConsider + 2])

        let (result, numberOfElementsReduced) =
            match (left, middle, right) with
            | (Number numLeft, Operator AdditionOperator, Number numRight) ->
                (numLeft + numRight, 3)
            | (Number numLeft, Operator MultiplicationOperator, Number numRight) ->
                (numLeft * numRight, 3)
            | (OpeningParenthesis, _, _) ->
                let innerExpression = getInnerExpression expression (indexToConsider)
                (evaluate2 innerExpression, innerExpression.Length + 2)
            | (Number numLeft, Operator AdditionOperator, OpeningParenthesis) ->
                let innerExpression = getInnerExpression expression (indexToConsider + 2)
                (numLeft + (evaluate2 innerExpression), 2 + innerExpression.Length + 2) // Left, middle, inner expression, parentheses
            | (Number numLeft, Operator MultiplicationOperator, OpeningParenthesis) ->
                let innerExpression = getInnerExpression expression (indexToConsider + 2)
                (numLeft * (evaluate2 innerExpression), 2 + innerExpression.Length + 2) // Left, middle, inner expression, parentheses
            | _ -> failwithf $"Could not evaluate {left} {middle} {right}"

        let leadingElements = expression |> Array.take indexToConsider
        let trailingElements = expression |> Array.skip (leadingElements.Length + numberOfElementsReduced)
        let newExpression = Array.append (Array.append leadingElements ([| Number result |])) (trailingElements)

        if newExpression.Length = 1 then
            result
        else
            evaluate2 newExpression

let partTwo () =
    input |> Array.map evaluate2 |> Array.sum
