module IntCode

type Memory = int list

type Pointer = Pointer of int
type ProgramInput = ProgramInput of int
type ProgramOutput = ProgramOutput of int

type ProgramState =
| Running of Memory * Pointer * ProgramInput option
| Complete of Memory * ProgramOutput option

let rec replace v i l = //v - value to substitute, i - index at which to substitute, l - the list
    // https://stackoverflow.com/a/23482571
    match i, l with
    | 0, x::xs -> v::xs //this line does the actual replace
    | i, x::xs -> x::replace v (i - 1) xs //simply iterates one further through the list
    | i, [] -> failwith "index out of range" // the given index is outside the bounds of the list

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
        | NinetyNine

        let parse rawCode =
            match rawCode with
            | 1 -> One
            | 2 -> Two
            | 3 -> Three
            | 4 -> Four
            | 99 -> NinetyNine
            | _ -> failwithf "Unhandled opcode %i" rawCode

        let paramCount code =
            match code with
            | One -> 3
            | Two -> 3
            | Three -> 1
            | Four -> 1
            | NinetyNine -> 0

    type T = {
        OpCode: OpCode.Type
        Params: Param.T list
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

        {
            OpCode = opCode
            Params = parameters
        }

    let invokeInstruction (currentState:ProgramState) (instruction:T) =
        let invokeOne (mem:Memory) (parameters:Param.T list) =
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

            let firstValue = Param.valueForReading mem parameters.[0]
            let secondValue = Param.valueForReading mem parameters.[1]

            let replacePosition = Param.valueForWriting parameters.[2]

            let newValue = firstValue + secondValue
            mem |> replace newValue replacePosition

        let invokeTwo (mem:Memory) (parameters:Param.T list) =
            // Defined in Day 2:
            //
            // Opcode 2 works exactly like opcode 1, except it multiplies the two inputs instead
            // of adding them. Again, the three integers after the opcode indicate where the
            // inputs and outputs are, not their values.

            let firstValue = Param.valueForReading mem parameters.[0]
            let secondValue = Param.valueForReading mem parameters.[1]

            let replacePosition = Param.valueForWriting parameters.[2]

            let newValue = firstValue * secondValue
            mem |> replace newValue replacePosition

        let invokeThree (mem:Memory) input (param:Param.T) =
            // Defined in Day 5:
            // Opcode 3 takes a single integer as input and saves it to the position given by
            // its only parameter. For example, the instruction 3,50 would take an input value
            // and store it at address 50.

            let (ProgramInput inputRaw) = input
            let outputPos = Param.valueForReading mem param

            mem |> replace inputRaw outputPos

        let invokeFour (mem:Memory) (param:Param.T) =
            // Defined in Day 5:
            // Opcode 4 outputs the value of its only parameter. For example, the instruction
            // 4,50 would output the value at address 50.

            let output = Param.valueForReading mem param |> ProgramOutput
            output

        match currentState with
        | Complete _ -> currentState
        | Running (mem, pointer, input) ->
            let (Pointer ptr) = pointer
            let newMemory, newOutput =
                match instruction.OpCode with
                | OpCode.Type.One ->
                    let newMem = invokeOne mem instruction.Params
                    (Some newMem), None
                | OpCode.Type.Two ->
                    let newMem = invokeTwo mem instruction.Params
                    (Some newMem), None
                | OpCode.Type.Three ->
                    match input with
                    | None -> failwith "No program input was provided. Opcode 3 requires program input."
                    | Some i ->
                        let newMem = invokeThree mem i instruction.Params.[0]
                        (Some newMem), None
                | OpCode.Type.Four ->
                    let output = invokeFour mem instruction.Params.[0]
                    (Some mem), (Some output)
                | OpCode.Type.NinetyNine ->
                    None, None

            let newPointer = ptr + (OpCode.paramCount instruction.OpCode) + 1 |> Pointer

            match newMemory with
            | Some s ->
                Running (s, newPointer, input)
            | None ->
                Complete (mem, newOutput)

module Program =
    [<Literal>]
    let private separator = ','

    let fromString (x:string) : Memory =
        x.Split([|separator|])
        |> Array.map int
        |> Array.toList

    let toString (x:Memory) =
        x
        |> List.map string
        |> String.concat (string separator)

    let rec private step state =
        match state with
        | Complete _ -> state
        | Running (memory, pointer, _) ->
            Instruction.getInstruction memory pointer
            |> Instruction.invokeInstruction state
            |> step

    let run (input:ProgramInput option) (memory:Memory) =
        let initialState = Running (memory, (Pointer 0), input)
        let result = step initialState
        match result with
        | Complete (mem,output) -> (mem, output)
        | Running (mem, pointer, _) ->
            let (Pointer ptr) = pointer
            failwithf "Failed to complete the program. Last pointer: %i Last memory: [%s]" ptr (toString mem)