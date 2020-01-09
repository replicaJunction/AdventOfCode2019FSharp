module PuzzleInput

open System
open System.IO

let private filePath day =
    let relativePath = sprintf "PuzzleInputs/Day%i.txt" day

    let expectedPath = Path.Combine(__SOURCE_DIRECTORY__, relativePath)
    if not (File.Exists(expectedPath)) then failwithf "Could not find puzzle input for day %i at path %s" day expectedPath
    expectedPath

let read day =
    let expectedPath = filePath day
    File.ReadAllText(expectedPath)

let readAsList day =
    let expectedPath = filePath day
    File.ReadAllLines(expectedPath) |> Array.toList
