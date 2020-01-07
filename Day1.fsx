// https://adventofcode.com/2019/day/1

open System

let checkFunc f input expected =
    let actual = f input
    if actual = expected then
        printfn "OK:      %O -> %O" input expected
    else
        printfn "Failed:  %O\n\tExpected: %O\n\tActual: %O" input expected actual

let puzzleInput = [
    120333
    142772
    85755
    90217
    74894
    86021
    66768
    147353
    67426
    145635
    100070
    88290
    110673
    109887
    91389
    121365
    52760
    58613
    130918
    57842
    80622
    50466
    80213
    85816
    149832
    133813
    60211
    69491
    129415
    141471
    77916
    98907
    63440
    109545
    80183
    143073
    77783
    88546
    149648
    128010
    55530
    54878
    103885
    57312
    81011
    148450
    137947
    67252
    106264
    149860
    71677
    101209
    128477
    112159
    56027
    53313
    118916
    98057
    131668
    61605
    107488
    65517
    63594
    84072
    79214
    141606
    137375
    112525
    64572
    126216
    57013
    130003
    122450
    50642
    136844
    96272
    97861
    59071
    106870
    116595
    144966
    88723
    124038
    63629
    105304
    52928
    92917
    147571
    120553
    113823
    85524
    71152
    95199
    102000
    118874
    133317
    146849
    60450
    103307
    117162
]

let minus x y = y - x

let fullSum f items =
    items
    |> List.map f
    |> List.sum

let fuelForModule mass =
    mass / 3
    |> float
    |> Math.Floor
    |> int
    |> minus 2

let rec fuelForModuleIncludingSelf mass =
    let baseFuel = fuelForModule mass

    let additionalFuel =
        if baseFuel <= 0 then
            0
        else

            let r = fuelForModuleIncludingSelf baseFuel
            if r < 0 then 0 else r

    baseFuel + additionalFuel

// Validate the test cases for part 1
let check = checkFunc fuelForModule

check 12 2
check 14 2
check 1969 654
check 100756 33583

printfn ""

// Solve the actual puzzle for part 1
let part1 = fullSum fuelForModule
printfn "Part 1 answer: %i" (part1 puzzleInput)

// Solve the actual puzzle for part 2
let part2 = fullSum fuelForModuleIncludingSelf
puzzleInput |> part2
printfn "Part 2 answer: %i" (part2 puzzleInput)

printfn ""