module IntCode

type Memory = int list

type ProgramState =
| Running of Memory * int
| Complete of Memory

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
    | NinetyNine

    let getInstruction (mem:Memory) (pointer:int) =
        let opCode = mem.[pointer]

        try
            match opCode with
            | 1 ->
                One (
                    mem.[pointer + 1],
                    mem.[pointer + 2],
                    mem.[pointer + 3]
                    )
            | 2 ->
                Two (
                    mem.[pointer + 1],
                    mem.[pointer + 2],
                    mem.[pointer + 3]
                    )
            | 99 ->
                NinetyNine
            | _ ->
                failwithf "Unknown opcode %i" opCode
        with
        | exn -> failwithf "Failed to handle opcode %i at pointer %i: %O" opCode pointer exn

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

        match currentState with
        | Complete _ -> currentState
        | Running (mem, pointer) ->
            let newMemory,newPointer =
                match instruction with
                | One (x,y,z) ->
                    let newMem = invokeOne mem (x, y, z)
                    let newPointer = pointer + 4 |> Some
                    newMem,newPointer
                | Two (x,y,z) ->
                    let newMem = invokeTwo mem (x, y, z)
                    let newPointer = pointer + 4 |> Some
                    newMem,newPointer
                | NinetyNine ->
                    // Defined in day 2, code 99 means the program should immediately terminate
                    mem,None

            match newPointer with
            | Some s ->
                Running (newMemory, s)
            | None ->
                Complete newMemory

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
        | Running (memory, pointer) ->
            Instruction.getInstruction memory pointer
            |> Instruction.invokeInstruction state
            |> step

    let run (memory:Memory) =
        let result = step (Running (memory, 0))
        match result with
        | Complete mem -> mem
        | Running (mem, pointer) -> failwithf "Failed to complete the program. Last pointer: %i Last memory: [%s]" pointer (toString mem)