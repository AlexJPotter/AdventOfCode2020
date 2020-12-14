module Day14

open FileHelpers

type Instruction = { MemoryAddress: int; Value: int64; }

let parseInstruction (inputLine: string) : Instruction =
    let parts = inputLine.Split(" = ")
    let value = parts.[1] |> int64
    let memPart = parts.[0]
    let memPartLength = memPart.Length
    let memoryAddressLength = memPartLength - "mem[".Length - "]".Length
    let memoryAddress = memPart.Substring("mem[".Length, memoryAddressLength) |> int
    { MemoryAddress = memoryAddress; Value = value }

let parseMask (inputLine: string) =
    inputLine.Substring("mask = ".Length) |> seq |> Seq.toArray

let testInputString = "mask = 000000000000000000000000000000X1001X
mem[42] = 100
mask = 00000000000000000000000000000000X0XX
mem[26] = 1"

let testInput = testInputString.Replace("\r\n", "\n").Split("\n")

let input = readLinesFromFile "Day14/input.txt"

let maxMemoryAddress =
    input
    |> Array.filter (fun line -> line.StartsWith("mem"))
    |> Array.map parseInstruction
    |> Array.map (fun instruction -> instruction.MemoryAddress)
    |> Array.max

let toBits (value: int64) : int[] =
    let bits : int[] = Array.zeroCreate(36)

    let rec setBit (significance: int, valueLeft: int64) =
        let placeValue = System.Math.Pow(float 2, float significance) |> int64
        if placeValue <= valueLeft then
            bits.SetValue(1, 35 - significance)
            if significance = 0 then bits else setBit(significance - 1, valueLeft - placeValue)
        else
            if significance = 0 then bits else setBit(significance - 1, valueLeft)

    setBit(35, value)

let fromBits (bits: int[]) : int64 =
    [|0..35|]
    |> Array.map (fun significance -> if bits.[35 - significance] = 0 then 0L else (System.Math.Pow(float 2, float significance) |> int64))
    |> Array.sum

let applyMask (value: int64, mask: char[]) =
    let getMaskedBit (bit: int, maskBit: char) =
        match maskBit with
        | '1' -> 1
        | '0' -> 0
        | _ -> bit

    let rawBits = value |> toBits
    let maskedBits = Array.zip (rawBits) (mask) |> Array.map getMaskedBit
    let maskedValue = maskedBits |> fromBits
    maskedValue

let applyInstruction (instruction: Instruction, memorySpace: int64[], mask: char[]) =
    let maskedValue = applyMask(instruction.Value, mask)
    memorySpace.SetValue(maskedValue, instruction.MemoryAddress - 1)
    ignore

let partOne () =
    let memorySpace : int64[] = Array.zeroCreate (maxMemoryAddress)

    let rec processInput (input: string[], mask: char[]) =
        if input.Length = 0 then
            ignore
        else
            let inputLine = input |> Array.head

            if inputLine.StartsWith("mask") then
                processInput(input |> Array.skip(1), parseMask(inputLine))
            else
                let instruction = parseInstruction(inputLine)
                applyInstruction(instruction, memorySpace, mask) |> ignore
                processInput(input |> Array.skip(1), mask)

    processInput(input, Array.create 36 'X') |> ignore

    memorySpace |> Array.sum

let fromBitsV2 (bits: char[]) : int64[] =
    let rec getPossibleValues (currentBits: char[]) =
        let firstFloatingBitIndex = currentBits |> Array.tryFindIndex (fun charBit -> charBit = 'X')
        if firstFloatingBitIndex.IsNone then
            [| (currentBits |> Array.map (fun charBit -> if charBit = '0' then 0 else 1) |> fromBits) |]
        else
            let copy1 = currentBits |> Array.copy
            copy1.SetValue('1', firstFloatingBitIndex.Value)
            let copy2 = currentBits |> Array.copy
            copy2.SetValue('0', firstFloatingBitIndex.Value)
            Array.concat [| getPossibleValues(copy1) ; getPossibleValues(copy2) |]

    getPossibleValues(bits)

let toChar (bit: int) =
    bit.ToString() |> seq |> Seq.head

let applyMaskV2 (memoryAddress: int64, mask: char[]) : int64[] =
    let getMaskedBit (bit: int, maskBit: char) =
        match maskBit with
        | '1' -> '1'
        | '0' -> bit |> toChar
        | _ -> 'X'

    let rawBits = memoryAddress |> toBits
    let maskedBits = Array.zip (rawBits) (mask) |> Array.map getMaskedBit
    let possibleValues = maskedBits |> fromBitsV2
    possibleValues

let applyInstructionV2 (instruction: Instruction, memorySpace: Map<int64, int64>, mask: char[]) =
    let setMemoryAddress(memoryAddress: int64, currentMemorySpace: Map<int64, int64>) =
        if currentMemorySpace.ContainsKey(memoryAddress) then
            currentMemorySpace.Remove(memoryAddress).Add(memoryAddress, instruction.Value)
        else
            currentMemorySpace.Add(memoryAddress, instruction.Value)

    let rec setMemoryAddresses(addresses: int64[], currentMemorySpace: Map<int64, int64>) =
        if addresses.Length = 0 then
            currentMemorySpace
        else
            let memoryAddress = addresses |> Array.head
            let newMemorySpace = setMemoryAddress(memoryAddress, currentMemorySpace)
            setMemoryAddresses(addresses |> Array.skip(1), newMemorySpace)

    let memoryAddresses = applyMaskV2(instruction.MemoryAddress |> int64, mask)
    setMemoryAddresses(memoryAddresses, memorySpace)

let partTwo () =
    let rec processInput (input: string[], mask: char[], currentMemorySpace: Map<int64, int64>) =
        if input.Length = 0 then
            currentMemorySpace
        else
            let inputLine = input |> Array.head

            if inputLine.StartsWith("mask") then
                processInput(input |> Array.skip(1), parseMask(inputLine), currentMemorySpace)
            else
                let instruction = parseInstruction(inputLine)
                let newMemorySpace = applyInstructionV2(instruction, currentMemorySpace, mask)
                processInput(input |> Array.skip(1), mask, newMemorySpace)

    let finalMemorySpace = processInput(input, Array.create 36 'X', Map.empty)

    finalMemorySpace |> Map.toArray |> Array.sumBy (fun (_, value) -> value)
