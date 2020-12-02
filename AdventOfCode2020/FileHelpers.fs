module FileHelpers

open System.IO

let readLinesFromFile localFilePath =
    let absoluteFilePath = "C:/workspace/AdventOfCode2020/AdventOfCode2020/" + localFilePath
    File.ReadAllLines absoluteFilePath
