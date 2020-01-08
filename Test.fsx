#r "bin/Debug/netcoreapp3.1/AdventOfCode2019.dll"

let prgm = IntCode.Program.fromString "3,0,4,0,99"
let memory, output = IntCode.Program.run (Some (IntCode.ProgramInput 5)) prgm
if output.IsSome then
    printfn "Output: %O" (output.Value)