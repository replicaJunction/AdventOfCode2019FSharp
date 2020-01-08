// Day 2: 1202 Program Alarm
// https://adventofcode.com/2019/day/2
//
// Rewritten to use the IntCode library

#r "bin/Debug/netcoreapp3.1/AdventOfCode2019.dll"

let puzzleInput = "1,0,0,3,1,1,2,3,1,3,4,3,1,5,0,3,2,10,1,19,1,19,9,23,1,23,13,27,1,10,27,31,2,31,13,35,1,10,35,39,2,9,39,43,2,43,9,47,1,6,47,51,1,10,51,55,2,55,13,59,1,59,10,63,2,63,13,67,2,67,9,71,1,6,71,75,2,75,9,79,1,79,5,83,2,83,13,87,1,9,87,91,1,13,91,95,1,2,95,99,1,99,6,0,99,2,14,0,0"

let run prgm =
    IntCode.Program.run [] prgm
    |> fst

module Part1 =
    let checkTestCases() =
        let check program expected =
            printfn "\nRunning program: %s" program

            let prgm = IntCode.Program.fromString program
            let result = run prgm |> IntCode.Program.toString

            if result = expected then
                printfn "OK: %s" result
            else
                printfn "FAIL: Expected [%s], actual [%s]" expected result

        check "1,9,10,3,2,3,11,0,99,30,40,50" "3500,9,10,70,2,3,11,0,99,30,40,50"
        check "1,0,0,0,99" "2,0,0,0,99"
        check "2,3,0,3,99" "2,3,0,6,99"
        check "2,4,4,5,99,0" "2,4,4,5,99,9801"
        check "1,1,1,4,99,5,6,0,99" "30,1,1,4,2,5,6,0,99"

        printfn ""

    let solve() =
        let puzzleProgram =
            IntCode.Program.fromString puzzleInput
            |> IntCode.replace 12 1
            |> IntCode.replace 2 2

        let result = run puzzleProgram |> List.head
        printfn "\nPart 1 solution: %i\n" result

module Part2 =
    let solve() =
        let puzzleProgram = IntCode.Program.fromString puzzleInput

        let tryValues noun verb input =
            let codes =
                puzzleProgram
                |> IntCode.replace noun 1
                |> IntCode.replace verb 2

            codes |> run |> List.head

        let desiredOutput = 19690720
        let noun,verb =
            [0..99]
            |> List.allPairs [0..99]
            |> List.find(fun (x,y) ->
                tryValues x y puzzleInput = desiredOutput
                )

        let result = 100 * noun + verb
        printfn "\nPart 2 solution: %i\n" result

Part1.checkTestCases()
Part1.solve()
Part2.solve()