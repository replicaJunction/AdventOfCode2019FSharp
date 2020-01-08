module IntCode

type Memory = int list

type Pointer = Pointer of int
type ProgramInput = ProgramInput of int
type ProgramOutput = ProgramOutput of int

type ProgramState =
| Running of Memory * Pointer
| Complete of Memory * ProgramOutput option

let rec replace v i l = //v - value to substitute, i - index at which to substitute, l - the list
    // https://stackoverflow.com/a/23482571
    match i, l with
    | 0, x::xs -> v::xs //this line does the actual replace
    | i, x::xs -> x::replace v (i - 1) xs //simply iterates one further through the list
    | i, [] -> failwith "index out of range" // the given index is outside the bounds of the list

module private Instruction =
    type ValidOpCodes =
    | One of int * int * int
    | Two of int * int * int
    | Three of ProgramInput * int
    | Four of int
    | NinetyNine

    let getInstruction (input: ProgramInput option) (mem:Memory) (pointer:Pointer) =
        let (Pointer ptr) = pointer
        let opCode = mem.[ptr]

        try
            match opCode with
            | 1 ->
                One (
                    mem.[ptr + 1],
                    mem.[ptr + 2],
                    mem.[ptr + 3]
                    )
            | 2 ->
                Two (
                    mem.[ptr + 1],
                    mem.[ptr + 2],
                    mem.[ptr + 3]
                    )
            | 3 ->
                match input with
                | None ->
                    failwithf
                        "No input was provided for opcode 3, which requires an input\n\tPointer position: %i\n\tCurrent memory: [%s]"
                        ptr
                        (mem |> List.map string |> String.concat ",")
                | Some s ->
                    Three (
                        s,
                        mem.[ptr + 1]
                        )
            | 4 ->
                Four (
                    mem.[ptr + 1]
                )
            | 99 ->
                NinetyNine
            | _ ->
                failwithf "Unknown opcode %i" opCode
        with
        | exn -> failwithf "Failed to handle opcode %i at pointer %i: %O" opCode ptr exn

    let invokeInstruction (currentState:ProgramState) (instruction:ValidOpCodes) =
        let invokeOne (mem:Memory) (x,y,z) =
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

            let firstPosition = x
            let secondPosition = y
            let replacePosition = z

            let firstValue = mem.[firstPosition]
            let secondValue = mem.[secondPosition]
            let newValue = firstValue + secondValue

            mem |> replace newValue replacePosition

        let invokeTwo (mem:Memory) (x,y,z) =
            // Defined in Day 2:
            //
            // Opcode 2 works exactly like opcode 1, except it multiplies the two inputs instead
            // of adding them. Again, the three integers after the opcode indicate where the
            // inputs and outputs are, not their values.

            let firstPosition = x
            let secondPosition = y
            let replacePosition = z

            let firstValue = mem.[firstPosition]
            let secondValue = mem.[secondPosition]
            let newValue = firstValue * secondValue

            mem |> replace newValue replacePosition

        let invokeThree (mem:Memory) input outputPos =
            // Defined in Day 5:
            // Opcode 3 takes a single integer as input and saves it to the position given by
            // its only parameter. For example, the instruction 3,50 would take an input value
            // and store it at address 50.

            let (ProgramInput inputRaw) = input
            mem |> replace inputRaw outputPos

        let invokeFour (mem:Memory) param =
            // Defined in Day 5:
            // Opcode 4 outputs the value of its only parameter. For example, the instruction
            // 4,50 would output the value at address 50.

            let output = mem.[param] |> ProgramOutput
            mem, output

        match currentState with
        | Complete _ -> currentState
        | Running (mem, pointer) ->
            let newMemory ,newPointer, newOutput =
                let (Pointer ptr) = pointer
                match instruction with
                | One (x,y,z) ->
                    let newMem = invokeOne mem (x, y, z)
                    let newPointer = ptr + 4 |> Pointer |> Some
                    newMem, newPointer, None
                | Two (x,y,z) ->
                    let newMem = invokeTwo mem (x, y, z)
                    let newPointer = ptr + 4 |> Pointer |> Some
                    newMem, newPointer, None
                | Three (i,x) ->
                    let newMem = invokeThree mem i x
                    let newPointer = ptr + 2 |> Pointer |> Some
                    newMem, newPointer, None
                | Four x ->
                    let newMem, output = invokeFour mem x
                    let newPointer = ptr + 2 |> Pointer |> Some
                    newMem, newPointer, (Some output)
                | NinetyNine ->
                    // Defined in day 2, code 99 means the program should immediately terminate
                    mem, None, None

            match newPointer with
            | Some s ->
                Running (newMemory, s)
            | None ->
                Complete (newMemory, newOutput)

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

    let rec private step input state =
        match state with
        | Complete _ -> state
        | Running (memory, pointer) ->
            Instruction.getInstruction input memory pointer
            |> Instruction.invokeInstruction state
            |> step input

    let run (input:ProgramInput option) (memory:Memory) =
        let initialState = Running (memory, (Pointer 0))
        let result = step input initialState
        match result with
        | Complete (mem,output) -> (mem, output)
        | Running (mem, pointer) ->
            let (Pointer ptr) = pointer
            failwithf "Failed to complete the program. Last pointer: %i Last memory: [%s]" ptr (toString mem)