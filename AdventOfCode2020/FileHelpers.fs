module FileHelpers

open System.IO

let projectDirectoryPath = "C:/workspace/AdventOfCode2020/AdventOfCode2020/"

let readLinesFromFile (localFilePath: string) =
    File.ReadAllLines (projectDirectoryPath + localFilePath)

let readTextFromFile (localFilePath: string) =
    File.ReadAllText (projectDirectoryPath + localFilePath)
