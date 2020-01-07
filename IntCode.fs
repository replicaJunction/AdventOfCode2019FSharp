module IntCode

type Memory = int list

type ProgramState =
| Running of Memory * int
| Complete of Memory

module private Instruction =
    type ValidOpCodes =
    | One of int * int * int
    | Two of int * int * int
    | NinetyNine

    let rec private replace v i l = //v - value to substitute, i - index at which to substitute, l - the list
        // https://stackoverflow.com/a/23482571
        match i, l with
        | 0, x::xs -> v::xs //this line does the actual replace
        | i, x::xs -> x::replace v (i - 1) xs //simply iterates one further through the list
        | i, [] -> failwith "index out of range" // the given index is outside the bounds of the list

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
        match currentState with
        | Complete _ -> currentState
        | Running (mem, pointer) ->
            let newMemory,newPointer =
                match instruction with
                | One (x,y,z) ->
                    let newValue = mem.[x] + mem.[y]
                    printfn
                        "Running One instruction:\nX: %i (%i)\nY: %i (%i)\nZ: %i (%i) = %i"
                        x
                        mem.[x]
                        y
                        mem.[y]
                        z
                        mem.[z]
                        newValue

                    let newMem = mem |> replace newValue mem.[z]
                    let newPointer = pointer + 4 |> Some
                    newMem,newPointer
                | Two (x,y,z) ->
                    let newValue = mem.[x] * mem.[y]
                    printfn
                        "Running Two instruction:\nX: %i (%i)\nY: %i (%i)\nZ: %i (%i) = %i"
                        x
                        mem.[x]
                        y
                        mem.[y]
                        z
                        mem.[z]
                        newValue

                    let newMem = mem |> replace newValue mem.[z]
                    let newPointer = pointer + 4 |> Some
                    newMem,newPointer
                | NinetyNine ->
                    mem,None

            match newPointer with
            | Some s ->
                printfn
                    "Completed invokeInstruction:\n\tState: Running\n\tNew memory: [%O]\n\tNew pointer: %i"
                    (newMemory |> List.map string |> String.concat ",")
                    s
                Running (newMemory, s)
            | None ->
                printfn
                    "Completed invokeInstruction:\n\tState:Complete\n\tNew memory: %O"
                    (newMemory |> List.map string |> String.concat ",")
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

    let run (memory:Memory) = step (Running (memory, 0))