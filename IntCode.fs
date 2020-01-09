module IntCode

let IS_DEBUG_MODE = false

type Memory = int list
module Memory =
    [<Literal>]
    let private SEPARATOR = ','

    let fromString (x:string) : Memory =
        x.Split([|SEPARATOR|])
        |> Array.map int
        |> Array.toList

    let toString (x:Memory) =
        x
        |> List.map string
        |> String.concat (string SEPARATOR)

    let atPosition (pos:int) (x:Memory) = x.[pos]


type Pointer = Pointer of int
type ProgramInput = ProgramInput of int
type ProgramOutput = ProgramOutput of int

type ProgramState =
| Running
| AdvanceInstructionPointer
| Paused
| Complete
| Failed

type Program = {
    State: ProgramState
    Memory: Memory
    InstructionPointer: Pointer
    Inputs: ProgramInput list
    Outputs: ProgramOutput list
}

let rec replace v i l = //v - value to substitute, i - index at which to substitute, l - the list
    // https://stackoverflow.com/a/23482571
    match i, l with
    | 0, x::xs -> v::xs //this line does the actual replace
    | i, x::xs -> x::replace v (i - 1) xs //simply iterates one further through the list
    | i, [] -> failwith "index out of range" // the given index is outside the bounds of the list

// https://stackoverflow.com/a/10365848/4991083
let private dprintf fmt = Printf.kprintf (fun str ->
    if IS_DEBUG_MODE then printfn "%s" str) fmt

module Instruction =
    module Param =
        type Mode =
        | Position
        | Immediate

        type T = Mode * int

        let parseMode i =
            match i with
            | 0 -> Position
            | 1 -> Immediate
            | _ -> failwithf "Unknown parameter mode %i" i

        let create mode value = T (mode, value)
        let createDefault value = T (Position, value)

        let valueForReading (mem:Memory) (param:T) =
            let mode, value = param
            match mode with
            | Position ->
                // Defined in Day 5
                // In position mode, "if the parameter is 50, its value is the value stored
                // at address 50 in memory."
                mem.[value]
            | Immediate ->
                // Defined in Day 5
                // "In immediate mode, a parameter is interpreted as a value - if the parameter
                // is 50, its value is simply 50."
                value

        let valueForWriting (param:T) =
            // Per Day 5,
            // "Parameters that an instruction writes to will never be in immediate mode."
            // Per the author on Reddit, "If you write to a parameter in position mode, you
            // write to the given position. If that parameter is "17", you write to position
            // 17."
            //
            // From my point of view, that means we're just discarding the mode information,
            // because we're always using the value of the parameter as the position we're
            // writing to.
            //
            // See discussion here:
            // https://www.reddit.com/r/adventofcode/comments/e6d2g5/day_5_parameter_3_always_was_immediate/f9pd1fc/

            param |> snd


    module OpCode =
        type Type =
        | One
        | Two
        | Three
        | Four
        | Five
        | Six
        | Seven
        | Eight
        | NinetyNine

        let parse rawCode =
            match rawCode with
            | 1 -> One
            | 2 -> Two
            | 3 -> Three
            | 4 -> Four
            | 5 -> Five
            | 6 -> Six
            | 7 -> Seven
            | 8 -> Eight
            | 99 -> NinetyNine
            | _ -> failwithf "Unhandled opcode %i" rawCode

        let paramCount code =
            match code with
            | One -> 3
            | Two -> 3
            | Three -> 1
            | Four -> 1
            | Five -> 2
            | Six -> 2
            | Seven -> 3
            | Eight -> 3
            | NinetyNine -> 0

    [<RequireQualifiedAccess>]
    type T = {
        OpCode: OpCode.Type
        Params: Param.T list
    }

    let create opCode parameters = {
        T.OpCode = opCode
        T.Params = parameters
    }

    let getInstruction (mem:Memory) (pointer:Pointer) =
        let OP_CODE_LENGTH = 2

        let (Pointer ptr) = pointer

        let opCodeStr = mem.[ptr] |> string
        let opCode =
            let s =
                if opCodeStr.Length <= OP_CODE_LENGTH then
                    opCodeStr
                else
                    opCodeStr.Substring(opCodeStr.Length - OP_CODE_LENGTH)

            s
            |> int
            |> OpCode.parse

        let paramCount = OpCode.paramCount opCode

        let parameters =
            if paramCount = 0 then
                []
            else
                [1..paramCount]
                |> List.map (fun x ->
                    let mode =
                        let index = opCodeStr.Length - OP_CODE_LENGTH - x
                        if index >= 0 then
                            opCodeStr.Substring(index, 1) |> int |> Param.parseMode
                        else
                            Param.Mode.Position

                    let thisPtr = ptr + x
                    let value = mem.[thisPtr]

                    Param.create mode value
                    )

        create opCode parameters

module Program =
    open Instruction

    let invokeInstruction (program:Program) (instruction:Instruction.T) =

        // These helper functions don't need pararmeters, because they'll always refer
        // to the "program" and "instruction" variables.
        // However, we don't want them all to run every time, so we add a single Unit
        // parameter to each.

        let invokeOne () =
            // Defined in Day 2:
            //
            // Opcode 1 adds together numbers read from two positions and stores the result
            // in a third position. The three integers immediately after the opcode tell
            // you these three positions - the first two indicate the positions from which
            // you should read the input values, and the third indicates the position at which
            // the output should be stored.
            //
            // For example, if your Intcode computer encounters 1,10,20,30, it should read the
            // values at positions 10 and 20, add those values, and then overwrite the value
            // at position 30 with their sum.

            let mem = program.Memory
            let p = instruction.Params

            let firstValue = Param.valueForReading mem p.[0]
            let secondValue = Param.valueForReading mem p.[1]

            let replacePosition = Param.valueForWriting p.[2]

            let newValue = firstValue + secondValue
            let newMem = mem |> replace newValue replacePosition

            { program with Memory = newMem; State = AdvanceInstructionPointer }

        let invokeTwo () =
            // Defined in Day 2:
            //
            // Opcode 2 works exactly like opcode 1, except it multiplies the two inputs instead
            // of adding them. Again, the three integers after the opcode indicate where the
            // inputs and outputs are, not their values.

            let mem = program.Memory
            let p = instruction.Params

            let firstValue = Param.valueForReading mem p.[0]
            let secondValue = Param.valueForReading mem p.[1]

            let replacePosition = Param.valueForWriting p.[2]

            let newValue = firstValue * secondValue
            let newMem = mem |> replace newValue replacePosition

            { program with Memory = newMem; State = AdvanceInstructionPointer }

        let invokeThree () =
            // Defined in Day 5:
            // Opcode 3 takes a single integer as input and saves it to the position given by
            // its only parameter. For example, the instruction 3,50 would take an input value
            // and store it at address 50.

            let mem = program.Memory
            let (Pointer ptr) = program.InstructionPointer
            let input = program.Inputs
            let p = instruction.Params |> List.head

            match input with
            | [] ->
                // If there are not enough inputs provided, pause the program.
                // Note that we don't advance the instruction pointer, so when the program
                // resumes, it should restart on this instruction.

                dprintf "Opcode 3 encountered at index %i, but not enough inputs are present. Pausing." ptr

                { program with State = ProgramState.Paused }

            | (ProgramInput i) :: remainingInput ->

                let outputPos = Param.valueForWriting p
                let newMem = mem |> replace i outputPos

                { program with Memory = newMem; Inputs = remainingInput; State = AdvanceInstructionPointer }

        let invokeFour () =
            // Defined in Day 5:
            // Opcode 4 outputs the value of its only parameter. For example, the instruction
            // 4,50 would output the value at address 50.

            let p = instruction.Params |> List.head
            let output = Param.valueForReading program.Memory p |> ProgramOutput

            { program with Outputs = (program.Outputs @ [output]); State = AdvanceInstructionPointer }

        let invokeFive () =
            // Defined in Day 5, part 2:
            // Opcode 5 is jump-if-true: if the first parameter is non-zero, it sets
            // the instruction pointer to the value from the second parameter.
            // Otherwise, it does nothing.

            let mem = program.Memory
            let p = instruction.Params
            let firstValue = Param.valueForReading mem p.[0]

            if firstValue <> 0 then
                let newPointerPos = Param.valueForReading mem p.[1]

                // Don't advance the pointer if we jump
                { program with InstructionPointer = Pointer newPointerPos }
            else
                { program with State = ProgramState.AdvanceInstructionPointer }

        let invokeSix () =
            // Defined in Day 5, part 2:
            // Opcode 6 is jump-if-false: if the first parameter is zero, it sets
            // the instruction pointer to the value from the second parameter.
            // Otherwise, it does nothing.

            let mem = program.Memory
            let p = instruction.Params
            let firstValue = Param.valueForReading mem p.[0]

            if firstValue = 0 then
                let newPointerPos = Param.valueForReading mem p.[1]

                // Don't advance the pointer if we jump
                { program with InstructionPointer = Pointer newPointerPos }
            else
                { program with State = ProgramState.AdvanceInstructionPointer }

        let invokeSeven () =
            // Defined in Day 5, part 2:
            // Opcode 7 is less than: if the first parameter is less than the second
            // parameter, it stores 1 in the position given by the third parameter.
            // Otherwise, it stores 0.

            let mem = program.Memory
            let p = instruction.Params

            let firstValue = Param.valueForReading mem p.[0]
            let secondValue = Param.valueForReading mem p.[1]

            let newValue = if firstValue < secondValue then 1 else 0
            let outputPos = Param.valueForWriting p.[2]
            let newMem = mem |> replace newValue outputPos

            { program with Memory = newMem; State = AdvanceInstructionPointer }

        let invokeEight () =
            // Defined in Day 5, part 2:
            // Opcode 8 is equals: if the first parameter is equal to the second
            // parameter, it stores 1 in the position given by the third parameter.
            // Otherwise, it stores 0.

            let mem = program.Memory
            let p = instruction.Params

            let firstValue = Param.valueForReading mem p.[0]
            let secondValue = Param.valueForReading mem p.[1]

            let newValue = if firstValue = secondValue then 1 else 0
            let outputPos = Param.valueForWriting p.[2]
            let newMem = mem |> replace newValue outputPos

            { program with Memory = newMem; State = AdvanceInstructionPointer }

        let invokeNinetyNine () =
            // Defined in Day 2:
            // 99 means that the program is finished and should immediately halt.

            { program with State = Complete }

        // This one does need a parameter, because the program it will operate on is
        // the output of one of the opcode functions above, not the one provided to
        // this function.
        let advancePointer (program:Program) =
            match program.State with
            | AdvanceInstructionPointer ->
                let (Pointer ptr) = program.InstructionPointer
                let newPointer = ptr + (OpCode.paramCount instruction.OpCode) + 1

                { program with State = Running; InstructionPointer = Pointer newPointer }
            | _ -> program

        let f =
            match instruction.OpCode with
            | OpCode.Type.One -> invokeOne
            | OpCode.Type.Two -> invokeTwo
            | OpCode.Type.Three -> invokeThree
            | OpCode.Type.Four -> invokeFour
            | OpCode.Type.Five -> invokeFive
            | OpCode.Type.Six -> invokeSix
            | OpCode.Type.Seven -> invokeSeven
            | OpCode.Type.Eight -> invokeEight
            | OpCode.Type.NinetyNine -> invokeNinetyNine

        f ()
        |> advancePointer

    let rec private step (program:Program) =
        match program.State with
        | Running ->
            let (Pointer ptr) = program.InstructionPointer
            dprintf
                "Step\n\tState: %A\n\tMemory: %s\n\tPointer: %i"
                program.State
                (program.Memory |> Memory.toString)
                ptr

            Instruction.getInstruction program.Memory program.InstructionPointer
            |> invokeInstruction program
            |> step
        | _ ->
            // If the program isn't running (whether it's paused, failed, or complete),
            // do nothing

            dprintf "Exiting program with state %A" program.State
            program

    let create (input:int list) (programText:string) =
        let mem = programText |> Memory.fromString
        let inputs = input |> List.map ProgramInput

        {
            State = Running
            Memory = mem
            InstructionPointer = Pointer 0
            Inputs = inputs
            Outputs = List.empty
        }

    let run (program:Program) = step program

let runProgram program =
    Program.create [] program
    |> Program.run

let runProgramWithInput (input:int list) program =
    Program.create input program
    |> Program.run

let firstOutput (program:Program) =
    let (ProgramOutput realOutput) = program.Outputs |> List.head
    realOutput

let lastOutput (program:Program) =
    let (ProgramOutput realOutput) = program.Outputs |> List.last
    realOutput
