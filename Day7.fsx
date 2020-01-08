// Day 7: Amplification Circuit
// https://adventofcode.com/2019/day/7

#r "bin/Debug/netcoreapp3.1/AdventOfCode2019.dll"

// https://stackoverflow.com/a/286821/4991083
let permutations list =
    let rec permutationsRec list taken =
      seq { if Set.count taken = List.length list then yield [] else
            for l in list do
              if not (Set.contains l taken) then
                for perm in permutationsRec list (Set.add l taken)  do
                  yield l::perm }

    permutationsRec list Set.empty

let thrustForInputSequence program inputValues =
    let fold previousOutput currentPhase =
        let output =
            program
            |> IntCode.runProgramWithInput [currentPhase; previousOutput]
            |> IntCode.firstOutput

        printfn "Phase %i, previous output %i -> output %i" currentPhase previousOutput output
        output

    inputValues |> List.fold fold 0

let findBestInputSequence program =
    [0..4]
    |> permutations
    |> Seq.map (fun x ->
        let thisThrust = thrustForInputSequence program x
        x, thisThrust
        )

module Part1 =
    let check () =
        let checkSingle program input expected =
            let actual = thrustForInputSequence program input
            if expected = actual then
                printfn "OK: %i" expected
            else
                printfn "FAIL: expected %i but got %i" expected actual

        let checkBest program expectedOrder expectedThrust =
            let (order, thrust) = findBestInputSequence program
            if expected = actual then
                printfn "OK: %i" expected
            else
                printfn "FAIL: expected %i but got %i" expected actual

        let example1 = "3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0"
        let exampleSeq1 = [4;3;2;1;0]
        let exampleThrust1 = 43210

        let example2 = "3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0"
        let exampleSeq2 = [0;1;2;3;4]
        let exampleThrust2 = 54321

        let example3 = "3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0"
        let exampleSeq3 = [1;0;4;3;2]
        let exampleThrust3 = 65210

        checkSingle example1 exampleSeq1 exampleThrust1
        checkSingle example2 exampleSeq2 exampleThrust2
        checkSingle example3 exampleSeq3 exampleThrust3



Part1.check()