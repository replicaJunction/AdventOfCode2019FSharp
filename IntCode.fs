module IntCode

let IS_DEBUG_MODE = false

type Memory = int list

type Pointer = Pointer of int
type ProgramInput = ProgramInput of int
type ProgramOutput = ProgramOutput of int

type ProgramState =
| Running of Memory * Pointer * ProgramInput option * ProgramOutput list
| Complete of Memory * ProgramOutput list

let rec replace v i l = //v - value to substitute, i - index at which to substitute, l - the list
    // https://stackoverflow.com/a/23482571
    match i, l with
    | 0, x::xs -> v::xs //this line does the actual replace
    | i, x::xs -> x::replace v (i - 1) xs //simply iterates one further through the list
    | i, [] -> failwith "index out of range" // the given index is outside the bounds of the list

[<Literal>]
let private MEMORY_STRING_SEPARATOR = ','

let stringToMemory (x:string) : Memory =
    x.Split([|MEMORY_STRING_SEPARATOR|])
    |> Array.map int
    |> Array.toList

let memoryToString (x:Memory) =
    x
    |> List.map string
    |> String.concat (string MEMORY_STRING_SEPARATOR)

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

    // These properties are Some if they were modified by the instruction, and None if there
    // was no change by the instruction.
    type private InstructionResult = {
        Memory: Memory option
        Pointer: Pointer option
        Output: ProgramOutput option
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
            let newMem = mem |> replace newValue replacePosition

            {
                Memory = Some newMem
                Pointer = None
                Output = None
            }


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
            let newMem = mem |> replace newValue replacePosition

            {
                Memory = Some newMem
                Pointer = None
                Output = None
            }

        let invokeThree (mem:Memory) input (param:Param.T) =
            // Defined in Day 5:
            // Opcode 3 takes a single integer as input and saves it to the position given by
            // its only parameter. For example, the instruction 3,50 would take an input value
            // and store it at address 50.

            let (ProgramInput inputRaw) = input
            let outputPos = Param.valueForWriting param

            let newMem = mem |> replace inputRaw outputPos

            {
                Memory = Some newMem
                Pointer = None
                Output = None
            }

        let invokeFour (mem:Memory) (param:Param.T) =
            // Defined in Day 5:
            // Opcode 4 outputs the value of its only parameter. For example, the instruction
            // 4,50 would output the value at address 50.

            let output = Param.valueForReading mem param |> ProgramOutput

            {
                Memory = None
                Pointer = None
                Output = Some output
            }

        let invokeFive (mem:Memory) (parameters:Param.T list) =
            // Defined in Day 5, part 2:
            // Opcode 5 is jump-if-true: if the first parameter is non-zero, it sets
            // the instruction pointer to the value from the second parameter.
            // Otherwise, it does nothing.

            let firstValue = Param.valueForReading mem parameters.[0]
            let newPointer =
                if firstValue <> 0 then
                    let newPointerPos = Param.valueForReading mem parameters.[1]
                    Some (Pointer newPointerPos)
                else
                    None

            {
                Memory = None
                Pointer = newPointer
                Output = None
            }

        let invokeSix (mem:Memory) (parameters:Param.T list) =
            // Defined in Day 5, part 2:
            // Opcode 6 is jump-if-false: if the first parameter is zero, it sets
            // the instruction pointer to the value from the second parameter.
            // Otherwise, it does nothing.

            let firstValue = Param.valueForReading mem parameters.[0]
            let newPointer =
                if firstValue = 0 then
                    let newPointerPos = Param.valueForReading mem parameters.[1]
                    Some (Pointer newPointerPos)
                else
                    None

            {
                Memory = None
                Pointer = newPointer
                Output = None
            }

        let invokeSeven (mem:Memory) (parameters:Param.T list) =
            // Defined in Day 5, part 2:
            // Opcode 7 is less than: if the first parameter is less than the second
            // parameter, it stores 1 in the position given by the third parameter.
            // Otherwise, it stores 0.

            let firstValue = Param.valueForReading mem parameters.[0]
            let secondValue = Param.valueForReading mem parameters.[1]

            let newValue = if firstValue < secondValue then 1 else 0
            let outputPos = Param.valueForWriting parameters.[2]
            let newMem = mem |> replace newValue outputPos

            {
                Memory = Some newMem
                Pointer = None
                Output = None
            }

        let invokeEight (mem:Memory) (parameters:Param.T list) =
            // Defined in Day 5, part 2:
            // Opcode 8 is equals: if the first parameter is equal to the second
            // parameter, it stores 1 in the position given by the third parameter.
            // Otherwise, it stores 0.

            let firstValue = Param.valueForReading mem parameters.[0]
            let secondValue = Param.valueForReading mem parameters.[1]

            let newValue = if firstValue = secondValue then 1 else 0
            let outputPos = Param.valueForWriting parameters.[2]
            let newMem = mem |> replace newValue outputPos

            {
                Memory = Some newMem
                Pointer = None
                Output = None
            }

        match currentState with
        | Complete _ -> currentState
        | Running (mem, pointer, input, previousOutputs) ->
            let (Pointer ptr) = pointer

            dprintf
                "\nMemory: [%s]\nInstruction pointer: %i\nCurrent instruction: %O"
                (memoryToString mem)
                ptr
                instruction

            let result =
                match instruction.OpCode with
                | OpCode.Type.One -> invokeOne mem instruction.Params |> Some
                | OpCode.Type.Two -> invokeTwo mem instruction.Params |> Some
                | OpCode.Type.Three ->
                    match input with
                    | None -> failwith "No program input was provided. Opcode 3 requires program input."
                    | Some i -> invokeThree mem i instruction.Params.[0] |> Some
                | OpCode.Type.Four -> invokeFour mem instruction.Params.[0] |> Some
                | OpCode.Type.Five -> invokeFive mem instruction.Params |> Some
                | OpCode.Type.Six -> invokeSix mem instruction.Params |> Some
                | OpCode.Type.Seven -> invokeSeven mem instruction.Params |> Some
                | OpCode.Type.Eight -> invokeEight mem instruction.Params |> Some
                | OpCode.Type.NinetyNine -> None

            match result with
            | None -> Complete (mem, previousOutputs)
            | Some r ->
                let newMem =
                    match r.Memory with
                    | Some s -> s
                    | None -> mem

                let newPointer =
                    match r.Pointer with
                    | Some s -> s
                    | None ->
                        // As explained in Day 5, part 2:
                        //
                        // Normally, after an instruction is finished, the instruction pointer
                        // increases by the number of values in that instruction. However, if the
                        // instruction modifies the instruction pointer, that value is used and the
                        // instruction pointer is not automatically increased.

                        ptr + (OpCode.paramCount instruction.OpCode) + 1 |> Pointer

                let newOutputs =
                    match r.Output with
                    | Some s -> previousOutputs @ [s]
                    | None -> previousOutputs

                Running (newMem, newPointer, input, newOutputs)

module Program =
    let fromString (x:string) : Memory = stringToMemory x
    let toString (x:Memory) = memoryToString x

    let rec private step state =
        match state with
        | Complete _ -> state
        | Running (memory, pointer, _, _) ->
            Instruction.getInstruction memory pointer
            |> Instruction.invokeInstruction state
            |> step

    let run (input:ProgramInput option) (memory:Memory) =
        let initialState = Running (memory, (Pointer 0), input, List.empty)
        let result = step initialState
        match result with
        | Complete (mem,output) -> (mem, output)
        | Running (mem, pointer, _, outputs) ->
            let (Pointer ptr) = pointer
            failwithf
                "Failed to complete the program.\n\tLast pointer: %i\n\tLast memory: [%s]\n\tOutputs: %s"
                ptr
                (toString mem)
                (outputs |> List.map (fun (ProgramOutput value) -> string value) |> String.concat ",")