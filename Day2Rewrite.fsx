// Day 2: 1202 Program Alarm
// https://adventofcode.com/2019/day/2
//
// Rewritten to use the IntCode library

#r @"D:\Documents\Projects\AdventOfCode2019FSharp\bin/Debug/netcoreapp3.1/AdventOfCode2019.dll"
//#r "bin/Debug/netcoreapp3.1/AdventOfCode2019.dll"

let puzzleInput = "1,0,0,3,1,1,2,3,1,3,4,3,1,5,0,3,2,10,1,19,1,19,9,23,1,23,13,27,1,10,27,31,2,31,13,35,1,10,35,39,2,9,39,43,2,43,9,47,1,6,47,51,1,10,51,55,2,55,13,59,1,59,10,63,2,63,13,67,2,67,9,71,1,6,71,75,2,75,9,79,1,79,5,83,2,83,13,87,1,9,87,91,1,13,91,95,1,2,95,99,1,99,6,0,99,2,14,0,0"

module Part1 =
    let checkTestCases() =
        let check program expected =
            printfn "\nRunning program: %s" program

            let prgm = IntCode.Program.fromString program
            match IntCode.Program.run prgm with
            | IntCode.Complete mem ->
                let memStr = mem |> IntCode.Program.toString
                if memStr = expected then
                    printfn "OK: %s" memStr
                else
                    printfn "FAIL: Expected [%s], actual [%s]" expected memStr
            | _ ->
                failwithf "Did not successfully complete the program"

        check "1,9,10,3,2,3,11,0,99,30,40,50" "3500,9,10,70,2,3,11,0,99,30,40,50"

        printfn ""

Part1.checkTestCases()