module Day06

open FileHelpers

let partOne () =
    let input =
        (readTextFromFile "Day06/input.txt")
            .Replace("\r\n", "\n")
            .Split("\n\n")
        |> Array.map (fun x -> x.Replace("\n", ""))

    let countUniqueCharacters (input: string) =
        seq input |> Seq.distinct |> Seq.length

    input |> Array.map countUniqueCharacters |> Array.sum

let partTwo () =
    let input =
        (readTextFromFile "Day06/input.txt")
            .Replace("\r\n", "\n")
            .Split("\n\n")
        |> Array.map (fun group -> group.Split("\n") |> Array.filter (fun line -> line.Length > 0))

    let getCountForGroup (groupLines: string []) =
        groupLines |> Array.map (fun line -> seq line |> Set.ofSeq) |> Set.intersectMany |> Set.count

    input |> Array.map getCountForGroup |> Array.sum
