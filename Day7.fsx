// Day 7: Amplification Circuit
// https://adventofcode.com/2019/day/7

#r "bin/Debug/netcoreapp3.1/AdventOfCode2019.dll"

let puzzleInput = "3,8,1001,8,10,8,105,1,0,0,21,30,51,72,81,94,175,256,337,418,99999,3,9,101,5,9,9,4,9,99,3,9,1001,9,3,9,1002,9,2,9,1001,9,2,9,1002,9,5,9,4,9,99,3,9,1002,9,4,9,101,4,9,9,102,5,9,9,101,3,9,9,4,9,99,3,9,1002,9,4,9,4,9,99,3,9,102,3,9,9,1001,9,4,9,4,9,99,3,9,1001,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,101,1,9,9,4,9,3,9,101,1,9,9,4,9,3,9,101,1,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,1002,9,2,9,4,9,99,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,101,1,9,9,4,9,3,9,101,2,9,9,4,9,3,9,102,2,9,9,4,9,99,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,99,3,9,1001,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,1001,9,1,9,4,9,3,9,102,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,101,2,9,9,4,9,99,3,9,101,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,1001,9,1,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,101,1,9,9,4,9,99"

// https://stackoverflow.com/a/286821/4991083
let permutations list =
    let rec permutationsRec list taken =
      seq { if Set.count taken = List.length list then yield [] else
            for l in list do
              if not (Set.contains l taken) then
                for perm in permutationsRec list (Set.add l taken)  do
                  yield l::perm }

    permutationsRec list Set.empty

let orderStr order =
    // Helper function to arrange a list of ints into a string
    order
    |> List.map string
    |> String.concat ","
    |> sprintf "[%s]"

let findBestInputSequence f inputRange program =
    let ordersAndValues =
        inputRange
        |> permutations
        |> Seq.map (fun x ->
            let thisThrust = f program x
            x, thisThrust
            )

    let bestOrder, bestValue = ordersAndValues |> Seq.maxBy(fun (_, value) -> value)
    //printfn
    //    "Best order: %s\nBest thrust value: %i"
    //    (bestOrder |> orderStr)
    //    bestValue

    bestOrder, bestValue

let thrustForInputSequence program phaseOrder =
    // Run a program several times with several input values
    // and get the first output.
    //
    // phaseOrder is an int list representing the order the phases should be
    // tried. For example, [0;1;2;3;4] would run the program five times, using
    // 0 for the first phase, 1 for the second, and so on.
    //
    // We use List.fold here because we need to track the previous output -
    // and use it as the second input for the next run of the program.

    let fold previousOutput currentPhase =
        let output =
            program
            |> IntCode.runProgramWithInput [currentPhase; previousOutput]
            |> IntCode.firstOutput

        //printfn "Phase %i, previous output %i -> output %i" currentPhase previousOutput output
        output

    // Per the puzzle, use 0 for the first run of the program where normally it would accept
    // the output from the previous run.

    phaseOrder |> List.fold fold 0

let thrustForInputSequenceFeedback program phaseOrder =

    let rec replace v i l = //v - value to substitute, i - index at which to substitute, l - the list
        // https://stackoverflow.com/a/23482571
        match i, l with
        | 0, x::xs -> v::xs //this line does the actual replace
        | i, x::xs -> x::replace v (i - 1) xs //simply iterates one further through the list
        | i, [] -> failwith "index out of range" // the given index is outside the bounds of the list

    let mutable programs = phaseOrder |> List.map (fun x -> IntCode.Program.create [x] program)
    let mutable isRunning = true
    let mutable index = 0
    let maxIndex = programs.Length - 1
    let mutable result = 0

    // Helper function to update a single program
    let update newProgram index = programs |> replace newProgram index

    // Add a 0 to the first program's input
    let modifiedFirstProgram = { programs.[0] with Inputs = (programs.[0].Inputs @ [(IntCode.ProgramInput 0)]) }
    programs <- update modifiedFirstProgram index

    while isRunning do
        let thisProgram =
            let p = programs.[index]
            match p.State with
            | IntCode.ProgramState.Running
            | IntCode.ProgramState.Paused
                ->
                    //printfn
                    //    "Running program %i with inputs [%s]"
                    //    index
                    //    (p.Inputs |> List.map (fun (IntCode.ProgramInput p) -> string p) |> String.concat ",")

                    IntCode.Program.run p

            | IntCode.ProgramState.Complete -> p
            | _ ->
                let (IntCode.Pointer ptr) = p.InstructionPointer
                printfn
                    "ERROR: bad program state %A\n\tMemory: %s\n\tPointer: %i"
                    p.State
                    (IntCode.Memory.toString p.Memory)
                    ptr

                failwithf "Unhandled program state %A" p.State

        let currentOutput = thisProgram |> IntCode.lastOutput
        //printfn "Program %i is in state %A with latest output %i" index thisProgram.State currentOutput

        if thisProgram.State = IntCode.ProgramState.Complete && index >= maxIndex then
            result <- thisProgram |> IntCode.lastOutput
            isRunning <- false
        else
            let nextIndex = if index >= maxIndex then 0 else index + 1
            let newInputs = programs.[nextIndex].Inputs @ [(IntCode.ProgramInput currentOutput)]
            let newProgram = { programs.[nextIndex] with Inputs = newInputs }

            programs <- update newProgram nextIndex
            index <- nextIndex

    result



module Part1 =
    let check () =
        let checkSingle program input expected =
            let actual = thrustForInputSequence program input
            if expected = actual then
                printfn "OK: %i" expected
            else
                printfn "FAIL: expected %i but got %i" expected actual

        let checkBest program expectedOrder expectedThrust =
            let (order, thrust) = findBestInputSequence thrustForInputSequence [0..4] program
            if expectedOrder = order then
                printfn "Order is correct: %s" (orderStr order)
            else
                printfn "FAIL: expected order %s but got %s" (orderStr expectedOrder) (orderStr order)

            if expectedThrust = thrust then
                printfn "Thrust is correct: %i" thrust
            else
                printfn "FAIL: expected thrust %i but got %i" expectedThrust thrust

        let example1 = "3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0"
        let exampleSeq1 = [4;3;2;1;0]
        let exampleThrust1 = 43210

        let example2 = "3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0"
        let exampleSeq2 = [0;1;2;3;4]
        let exampleThrust2 = 54321

        let example3 = "3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0"
        let exampleSeq3 = [1;0;4;3;2]
        let exampleThrust3 = 65210

        printfn "\nChecking single results for thrust values\n"

        checkSingle example1 exampleSeq1 exampleThrust1
        checkSingle example2 exampleSeq2 exampleThrust2
        checkSingle example3 exampleSeq3 exampleThrust3

        printfn "\nChecking best orders\n"
        checkBest example1 exampleSeq1 exampleThrust1
        checkBest example2 exampleSeq2 exampleThrust2
        checkBest example3 exampleSeq3 exampleThrust3

    let solve() =
        let order, thrust = findBestInputSequence thrustForInputSequence [0..4] puzzleInput
        printfn "\nPart 1 solution: %i (order: %s)" thrust (orderStr order)

module Part2 =
    let check () =
        let checkSingle program input expected =
            let actual = thrustForInputSequenceFeedback program input
            if expected = actual then
                printfn "OK: %i" expected
            else
                printfn "FAIL: expected %i but got %i" expected actual

        let checkBest program expectedOrder expectedThrust =
            let (order, thrust) = findBestInputSequence thrustForInputSequenceFeedback [5..9] program
            if expectedOrder = order then
                printfn "Order is correct: %s" (orderStr order)
            else
                printfn "FAIL: expected order %s but got %s" (orderStr expectedOrder) (orderStr order)

            if expectedThrust = thrust then
                printfn "Thrust is correct: %i" thrust
            else
                printfn "FAIL: expected thrust %i but got %i" expectedThrust thrust

        let example1 = "3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5"
        let order1 = [9;8;7;6;5]
        let thrust1 = 139629729

        let example2 = "3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,-5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10"
        let order2 = [9;7;8;5;6]
        let thrust2 = 18216

        printfn "\nChecking single results for thrust values\n"

        checkSingle example1 order1 thrust1
        checkSingle example2 order2 thrust2

        printfn "\nChecking best orders\n"

        checkBest example1 order1 thrust1
        checkBest example2 order2 thrust2

    let solve() =
        let order, thrust = findBestInputSequence thrustForInputSequenceFeedback [5..9] puzzleInput
        printfn "\nPart 2 solution: %i (order %s)" thrust (orderStr order)

//Part1.check()
//Part1.solve()
Part2.check()
Part2.solve()
