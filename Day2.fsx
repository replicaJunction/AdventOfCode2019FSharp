// https://adventofcode.com/2019/day/2

let checkFunc f input expected =
    let actual = f input
    if actual = expected then
        printfn "OK:      %O -> %O" input expected
    else
        printfn "Failed:  %O\n\tExpected: %O\n\tActual: %O" input expected actual

let isDebug = false

let puzzleInput = "1,0,0,3,1,1,2,3,1,3,4,3,1,5,0,3,2,10,1,19,1,19,9,23,1,23,13,27,1,10,27,31,2,31,13,35,1,10,35,39,2,9,39,43,2,43,9,47,1,6,47,51,1,10,51,55,2,55,13,59,1,59,10,63,2,63,13,67,2,67,9,71,1,6,71,75,2,75,9,79,1,79,5,83,2,83,13,87,1,9,87,91,1,13,91,95,1,2,95,99,1,99,6,0,99,2,14,0,0"

let rec replace v i l = //v - value to substitute, i - index at which to substitute, l - the list
    // https://stackoverflow.com/a/23482571
    match i, l with
    | 0, x::xs -> v::xs //this line does the actual replace
    | i, x::xs -> x::replace v (i - 1) xs //simply iterates one further through the list
    | i, [] -> failwith "index out of range" // the given index is outside the bounds of the list

let codesFromString (str:string) =
    str.Split([|','|])
    |> Array.map int
    |> Array.toList

let codesToString (codes:int list) =
    codes
    |> List.map string
    |> String.concat ","

let rec processCodes (pos:int) (codes:int list) =
    //printfn ""
    if pos >= codes.Length then
        //printfn "No more codes to process"
        codes
    else
        //printfn "Current list: %s" (codes |> codesToString)
        let opCode = codes.[pos]
        if opCode = 99 then
            //printfn "Opcode 99: stop processing"
            codes
        else
            let firstPos = codes.[pos + 1]
            let secondPos = codes.[pos + 2]
            let newPos = codes.[pos + 3]

            //printfn "Opcode: %i\nFirst index: %i\nSecond index: %i\nThird index: %i"
            //    opCode
            //    firstPos
            //    secondPos
            //    newPos

            let firstValue = codes.[firstPos]
            let secondValue = codes.[secondPos]
            let newValue =
                if opCode = 1 then
                    firstValue + secondValue
                else if opCode = 2 then
                    firstValue * secondValue
                else
                    failwithf "Unhandled opcode %i" opCode

            //printfn "First value: %i\nSecond value:%i\nNew value:%i"
            //    firstValue
            //    secondValue
            //    newValue

            codes
            |> replace newValue newPos
            |> processCodes (pos + 4)

let intCodeProgram (program:string) =
    program
    |> codesFromString
    |> processCodes 0
    |> codesToString


printfn ""

// Validate the test case for day 1
let check program =
    printfn ""
    checkFunc intCodeProgram program

check "1,9,10,3,2,3,11,0,99,30,40,50" "3500,9,10,70,2,3,11,0,99,30,40,50"
check "1,0,0,0,99" "2,0,0,0,99"
check "2,3,0,3,99" "2,3,0,6,99"
check "2,4,4,5,99,0" "2,4,4,5,99,9801"
check "1,1,1,4,99,5,6,0,99" "30,1,1,4,2,5,6,0,99"

printfn ""

// Solve the part 1 puzzle
let part1 =
    let input =
        puzzleInput
        |> codesFromString
        |> replace 12 1
        |> replace 2 2

    input
    |> processCodes 0
    |> List.head
    |> string

printfn "\n\nPart 1 solution:\t%s\n\n" part1

// Generalize part 1 for part 2
let tryValues noun verb input =
    let codes =
        input
        |> codesFromString
        |> replace noun 1
        |> replace verb 2

    codes |> processCodes 0 |> List.head

let part2 =
    let desiredOutput = 19690720

    let noun,verb =
        [0..99]
        |> List.allPairs [0..99]
        |> List.find(fun (x,y) ->
            tryValues x y puzzleInput = desiredOutput
            )

    100 * noun + verb

printfn "\n\nPart 2 solution:\t%i\n\n" part2