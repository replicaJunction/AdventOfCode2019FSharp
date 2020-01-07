// Day 4: Secure Container
// https://adventofcode.com/2019/day/4

let puzzleInputLiteral = "136760-595730"

let initialInputs =
    let x = puzzleInputLiteral.Split([|'-'|]) |> Array.map int
    [x.[0]..x.[1]]

// These are the original implementations of these two "filter"
// functions.
// After writing them both, I generalized them to the code after
// this comment block, but since it might look a little bit cryptic,
// I've left these two in place as "full" implementations of it.
// The generalized code below works out to be the same implementation.

//let hasTwoAdjacentNumbers (i:int) =
//    let s = string i

//    let folder state index =
//        if index + 1 >= s.Length then
//            // If we've reached the end of the string,
//            // there's no more to check
//            state
//        else
//            let c1 = s.[index]
//            let c2 = s.[index + 1]

//            // OR logic, because this must be true in any case
//            state || (c1 = c2)

//    // False until proven true
//    [0..s.Length]
//    |> List.fold folder false

//let hasNoDecreasingDigits (i:int) =
//    let s = string i

//    let folder state index =
//        if index + 1 >= s.Length then
//            state
//        else
//            let digit1 = s.Substring(index, 1) |> int
//            let digit2 = s.Substring(index + 1, 1) |> int

//            // AND logic, because this must be true in all cases
//            state && (digit1 <= digit2)

//    // True until proven false
//    [0..s.Length]
//    |> List.fold folder true

let filterAdjacentDigits f useAnd (i:int) =
    let s = string i
    let folder state index =
        if index + 1 >= s.Length then
            state
        else
            let digit1 = s.Substring(index, 1) |> int
            let digit2 = s.Substring(index + 1, 1) |> int
            let thisResult = f digit1 digit2

            if useAnd then
                state && thisResult
            else
                state || thisResult

    [0..s.Length]
    |> List.fold folder useAnd

let hasTwoAdjacentNumbers = filterAdjacentDigits (=) false
let hasNoDecreasingDigits = filterAdjacentDigits (<=) true

//// Simple test cases

//hasTwoAdjacentNumbers 112345     // true
//hasTwoAdjacentNumbers 123456789  // false
//hasTwoAdjacentNumbers 1234567899 // true
//hasNoDecreasingDigits 123456789  // true
//hasNoDecreasingDigits 11223344   // true
//hasNoDecreasingDigits 123234     // false

let hasGroupOfExactlyTwoMatching (i:int) =
    let s = string i

    let folder state index =
        if index + 1 >= s.Length then
            // If we've reached the end of the string,
            // there's no more to check
            state
        else
            let c1 = s.[index]
            let c2 = s.[index + 1]

            let thisState =
                if index > 0 && index + 2 < s.Length then
                    let c0 = s.[index - 1]
                    let c3 = s.[index + 2]
                    c0 <> c1 && c1 = c2 && c2 <> c3
                else if index > 0 then
                    let c0 = s.[index - 1]
                    c0 <> c1 && c1 = c2
                else if index + 2 < s.Length then
                    let c3 = s.[index + 2]
                    c1 = c2 && c2 <> c3
                else
                    c1 = c2

            // OR logic, because this must be true in any case
            state || thisState

    // False until proven true
    [0..s.Length]
    |> List.fold folder false

module Part1 =
    let private applyAllFilters items =
        items
        |> List.filter hasTwoAdjacentNumbers
        |> List.filter hasNoDecreasingDigits

    let checkTestCases () =
        let check i expected =
            let actual =
                applyAllFilters [i]
                |> List.isEmpty
                |> not

            if expected = actual then
                printfn "%i: OK (%O)" i expected
            else
                printfn "%i: FAILED (expected %O, actual %O)" i expected actual

        check 111111 true
        check 223450 false
        check 123789 false

        printfn ""

    let solve () =
        let count =
            initialInputs
            |> applyAllFilters
            |> List.length

        printfn "\nPart 1 solution: %i\n" count

module Part2 =
    let private applyAllFilters items =
        items
        |> List.filter hasTwoAdjacentNumbers
        |> List.filter hasNoDecreasingDigits
        |> List.filter hasGroupOfExactlyTwoMatching

    let checkTestCases () =
        let check i expected =
            let actual =
                applyAllFilters [i]
                |> List.isEmpty
                |> not

            if expected = actual then
                printfn "%i: OK (%O)" i expected
            else
                printfn "%i: FAILED (expected %O, actual %O)" i expected actual

        check 112233 true
        check 123444 false
        check 111122 true

        printfn ""

    let solve () =
        let count =
            initialInputs
            |> applyAllFilters
            |> List.length

        printfn "\nPart 2 solution: %i\n" count

//Part1.checkTestCases()
//Part1.solve()
Part2.checkTestCases()
Part2.solve()
