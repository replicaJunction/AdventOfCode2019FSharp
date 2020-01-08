// Day 5: Sunny with a Chance of Asteroids
// https://adventofcode.com/2019/day/5

#r "bin/Debug/netcoreapp3.1/AdventOfCode2019.dll"

let puzzleInput = "3,225,1,225,6,6,1100,1,238,225,104,0,101,20,183,224,101,-63,224,224,4,224,1002,223,8,223,101,6,224,224,1,223,224,223,1101,48,40,225,1101,15,74,225,2,191,40,224,1001,224,-5624,224,4,224,1002,223,8,223,1001,224,2,224,1,223,224,223,1101,62,60,225,1102,92,15,225,102,59,70,224,101,-885,224,224,4,224,1002,223,8,223,101,7,224,224,1,224,223,223,1,35,188,224,1001,224,-84,224,4,224,102,8,223,223,1001,224,2,224,1,223,224,223,1001,66,5,224,1001,224,-65,224,4,224,102,8,223,223,1001,224,3,224,1,223,224,223,1002,218,74,224,101,-2960,224,224,4,224,1002,223,8,223,1001,224,2,224,1,224,223,223,1101,49,55,224,1001,224,-104,224,4,224,102,8,223,223,1001,224,6,224,1,224,223,223,1102,43,46,225,1102,7,36,225,1102,76,30,225,1102,24,75,224,101,-1800,224,224,4,224,102,8,223,223,101,2,224,224,1,224,223,223,1101,43,40,225,4,223,99,0,0,0,677,0,0,0,0,0,0,0,0,0,0,0,1105,0,99999,1105,227,247,1105,1,99999,1005,227,99999,1005,0,256,1105,1,99999,1106,227,99999,1106,0,265,1105,1,99999,1006,0,99999,1006,227,274,1105,1,99999,1105,1,280,1105,1,99999,1,225,225,225,1101,294,0,0,105,1,0,1105,1,99999,1106,0,300,1105,1,99999,1,225,225,225,1101,314,0,0,106,0,0,1105,1,99999,1008,226,226,224,1002,223,2,223,1005,224,329,1001,223,1,223,8,226,677,224,102,2,223,223,1006,224,344,1001,223,1,223,1007,226,677,224,1002,223,2,223,1005,224,359,101,1,223,223,1008,677,226,224,102,2,223,223,1006,224,374,1001,223,1,223,1107,226,677,224,1002,223,2,223,1006,224,389,1001,223,1,223,107,677,677,224,1002,223,2,223,1006,224,404,101,1,223,223,1007,226,226,224,1002,223,2,223,1006,224,419,101,1,223,223,7,677,226,224,1002,223,2,223,1005,224,434,1001,223,1,223,1007,677,677,224,1002,223,2,223,1006,224,449,101,1,223,223,107,226,226,224,1002,223,2,223,1006,224,464,1001,223,1,223,1108,677,677,224,1002,223,2,223,1005,224,479,101,1,223,223,8,677,226,224,1002,223,2,223,1006,224,494,101,1,223,223,7,226,677,224,102,2,223,223,1005,224,509,1001,223,1,223,1107,677,226,224,102,2,223,223,1005,224,524,1001,223,1,223,1108,677,226,224,1002,223,2,223,1005,224,539,1001,223,1,223,1108,226,677,224,102,2,223,223,1006,224,554,101,1,223,223,108,226,677,224,102,2,223,223,1005,224,569,1001,223,1,223,8,677,677,224,1002,223,2,223,1005,224,584,101,1,223,223,108,677,677,224,1002,223,2,223,1005,224,599,1001,223,1,223,108,226,226,224,102,2,223,223,1006,224,614,101,1,223,223,1008,677,677,224,102,2,223,223,1006,224,629,1001,223,1,223,107,226,677,224,102,2,223,223,1006,224,644,101,1,223,223,1107,677,677,224,1002,223,2,223,1005,224,659,1001,223,1,223,7,226,226,224,1002,223,2,223,1005,224,674,101,1,223,223,4,223,99,226"

module Part1 =
    let solve() =
        let userInput = IntCode.ProgramInput 1

        let mem, outputs =
            IntCode.Program.fromString puzzleInput
            |> IntCode.Program.run (Some userInput)

        printfn "Complete"
        printfn "\tMemory: [%s]" (IntCode.Program.toString mem)

        if (List.isEmpty outputs) then
            printfn "\tNo outputs"
        else
            printfn "\tOutputs:"
            outputs
            |> List.map (fun (IntCode.ProgramOutput o) ->
                printfn "\t%i" o
                )
            |> ignore

module Part2 =
    let checkTests() =
        let checkSingleOutput program input expected =
            printf "    Input %3i:\t" input

            let actualInput = input |> IntCode.ProgramInput |> Some

            let (IntCode.ProgramOutput actual) =
                IntCode.Program.fromString program
                |> IntCode.Program.run actualInput
                |> snd
                |> List.head

            if expected = actual then
                printfn "\t+ OK: [%i]" actual
            else
                printfn "\t- FAIL: expected [%i] but received [%i]" expected actual

        // Example 1: should return 1 if (input = 8)
        let example1 =" 3,9,8,9,10,9,4,9,99,-1,8"
        printfn "Example 1: [%s]" example1
        checkSingleOutput example1 4 0
        checkSingleOutput example1 8 1

        // Example 2: should return 1 if (input < 8)
        let example2 = "3,9,7,9,10,9,4,9,99,-1,8"
        printfn "Example 2: [%s]" example2
        checkSingleOutput example2 4 1
        checkSingleOutput example2 8 0
        checkSingleOutput example2 9 0

        // Example 3: should return 1 if (input = 8) (tests immediate mode)
        let example3 = "3,3,1108,-1,8,3,4,3,99"
        printfn "Example 3: [%s]" example3
        checkSingleOutput example3 4 0
        checkSingleOutput example3 8 1

        // Example 4: should return 1 if (input < 8) (tests immediate mode)
        let example4 = "3,3,1107,-1,8,3,4,3,99"
        printfn "Example 4: [%s]" example4
        checkSingleOutput example4 4 1
        checkSingleOutput example4 8 0
        checkSingleOutput example4 9 0

        // Example 5: should return 0 if the input was 0 or 1 if the input was non-zero
        let example5 = "3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9"
        printfn "Example 5: [%s] " example5
        checkSingleOutput example5 0 0
        checkSingleOutput example5 -1 1
        checkSingleOutput example5 1 1
        checkSingleOutput example5 5 1

        // Example 6: same as above, but testing with immediate mode
        let example6 = "3,3,1105,-1,9,1101,0,0,12,4,12,99,1"
        printfn "Example 6: [%s]" example6
        checkSingleOutput example6 0 0
        checkSingleOutput example6 -1 1
        checkSingleOutput example6 1 1
        checkSingleOutput example6 5 1

        // Example 7: "here's a larger example"
        // Outputs 999 if (input < 8)
        // Outputs 1000 if (input = 8)
        // Outputs 1001 if (input > 8)
        let example7 = "3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99"
        printfn "Example 7"
        checkSingleOutput example7 -4 999
        checkSingleOutput example7 4 999
        checkSingleOutput example7 8 1000
        checkSingleOutput example7 9 1001
        checkSingleOutput example7 909 1001

    let solve() =
        let userInput = IntCode.ProgramInput 5

        let (IntCode.ProgramOutput solution) =
            IntCode.Program.fromString puzzleInput
            |> IntCode.Program.run (Some userInput)
            |> snd
            |> List.head

        printfn "Part 2 solution: %i" solution

//Part1.solve()
//Part2.checkTests()
Part2.solve()