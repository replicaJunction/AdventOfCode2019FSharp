module Testing

let check f debug (input, expected) =
    let actual = f input
    if debug then printfn "\nInput: %O\nExpected: %O\nActual: %O" input expected actual

    if expected = actual then
        printfn "OK: %O" expected
    else
        printfn "FAIL: expected %O but got %O" expected actual