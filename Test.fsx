#r "bin/Debug/netcoreapp3.1/AdventOfCode2019.dll"

let mem = [1002;4;3;4;33]
let ptr = IntCode.Pointer 0

IntCode.Instruction.getInstruction mem ptr