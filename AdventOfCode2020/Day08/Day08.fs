module Day08

open FileHelpers

let getInput () =
    readLinesFromFile "Day08/input.txt"

type Instruction = { Keyword: string; Operand: string }

let partOne () =
    let input = getInput()

    let parseInstruction (instruction: string) =
        { Keyword = instruction.Split(" ").[0]; Operand = instruction.Split(" ").[1] }

    let parseNumber (stringValue: string) =
        if stringValue.StartsWith("+") then
            stringValue.Substring(1) |> int
        else
            stringValue |> int

    let rec execute (instructions: string [], instructionPointer: int, accumulator: int, visitedInstructions: Set<int>) =
        printfn "At instruction %d\tAccumulator is %d" instructionPointer accumulator
        let instruction = parseInstruction (instructions.[instructionPointer])

        if (visitedInstructions.Contains(instructionPointer)) then
            None
        else
            match instruction.Keyword with
            | "nop" -> execute (instructions, instructionPointer + 1, accumulator, visitedInstructions.Add(instructionPointer))
            | "acc" -> execute (instructions, instructionPointer + 1, accumulator + parseNumber(instruction.Operand), visitedInstructions.Add(instructionPointer))
            | "jmp" -> execute (instructions, instructionPointer + parseNumber(instruction.Operand), accumulator, visitedInstructions.Add(instructionPointer))
            | _ -> None
    
    execute (input, 0, 0, Set.empty)

let partTwo () =
    let input = getInput()

    let parseInstruction (instruction: string) =
        { Keyword = instruction.Split(" ").[0]; Operand = instruction.Split(" ").[1] }

    let parseNumber (stringValue: string) =
        if stringValue.StartsWith("+") then
            stringValue.Substring(1) |> int
        else
            stringValue |> int

    let rec compiles (instructions: string [], instructionPointer: int, accumulator: int, visitedInstructions: Set<int>, log: bool, switchIndex: int) =
        if log then printfn "At instruction %d\tAccumulator is %d" instructionPointer accumulator

        if instructionPointer >= instructions.Length then
            true
        elif (visitedInstructions.Contains(instructionPointer)) then
            false
        else
            let instruction = parseInstruction (instructions.[instructionPointer])
            let keyword =
                if switchIndex = instructionPointer then
                    match instruction.Keyword with
                    | "nop" -> "jmp"
                    | "jmp" -> "nop"
                    | _ -> instruction.Keyword
                else
                    instruction.Keyword

            match keyword with
            | "nop" -> compiles (instructions, instructionPointer + 1, accumulator, visitedInstructions.Add(instructionPointer), log, switchIndex)
            | "acc" -> compiles (instructions, instructionPointer + 1, accumulator + parseNumber(instruction.Operand), visitedInstructions.Add(instructionPointer), log, switchIndex)
            | "jmp" -> compiles (instructions, instructionPointer + parseNumber(instruction.Operand), accumulator, visitedInstructions.Add(instructionPointer), log, switchIndex)
            | _ -> false
    
    let indexesToCheck = [|0..(input.Length - 1)|]

    let index = indexesToCheck |> Array.find (fun i -> compiles (input, 0, 0, Set.empty, false, i))

    compiles(input, 0, 0, Set.empty, true, index)
