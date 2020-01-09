module Glyph

type Pixel = Black | White | Transparent
module Pixel =
    let fromInt i =
        if i = 0 then Black
            else if i = 1 then White
            else Transparent

    let toInt p =
        match p with
        | Black -> 0
        | White -> 1
        | Transparent -> 2
    let render p =
        match p with
        | Black -> '#'
        | White -> '.'
        | Transparent -> ' '

[<RequireQualifiedAccess>]
type T = string list

let glyphs =
    let a = [
        ".##.."
        "#..#."
        "#..#."
        "####."
        "#..#."
        "#..#."
    ]

    let b = [
        "###.."
        "#..#."
        "###.."
        "#..#."
        "#..#."
        "###.."
    ]

    let c = [
        ".##.."
        "#..#."
        "#...."
        "#...."
        "#..#."
        ".##.."
    ]

    let e = [
        "####."
        "#...."
        "###.."
        "#...."
        "#...."
        "####."
    ]

    let f = [
        "####."
        "#...."
        "###.."
        "#...."
        "#...."
        "#...."
    ]

    let g = [
        ".##.."
        "#..#."
        "#...."
        "#.##."
        "#..#."
        ".###."
    ]

    let h = [
        "#..#."
        "#..#."
        "####."
        "#..#."
        "#..#."
        "#..#."
    ]

    let j = [
        "..##."
        "...#."
        "...#."
        "...#."
        "#..#."
        ".##.."
    ]

    let k = [
        "#..#."
        "#.#.."
        "##..."
        "#.#.."
        "#.#.."
        "#..#."
    ]

    let l = [
        "#...."
        "#...."
        "#...."
        "#...."
        "#...."
        "####."
    ]

    let o = [
        ".##.."
        "#..#."
        "#..#."
        "#..#."
        "#..#."
        ".##.."
    ]

    let p = [
        "###.."
        "#..#."
        "#..#."
        "###.."
        "#...."
        "#...."
    ]

    let r = [
        "###.."
        "#..#."
        "#..#."
        "###.."
        "#.#.."
        "#..#."
    ]

    let s = [
        "..###"
        ".#..."
        ".#..."
        "..##."
        "....#"
        ".###."
    ]

    let u = [
        "#..#."
        "#..#."
        "#..#."
        "#..#."
        "#..#."
        ".##.."
    ]

    let y = [
        "#...#"
        "#...#"
        ".#.#."
        "..#.."
        "..#.."
        "..#.."
    ]

    let z = [
        "####."
        "...#."
        "..#.."
        ".#..."
        "#...."
        "####."
    ]

    Map.empty
    |> Map.add a 'A'
    |> Map.add b 'B'
    |> Map.add c 'C'
    |> Map.add e 'E'
    |> Map.add f 'F'
    |> Map.add g 'G'
    |> Map.add h 'H'
    |> Map.add j 'J'
    |> Map.add k 'K'
    |> Map.add l 'L'
    |> Map.add o 'O'
    |> Map.add p 'P'
    |> Map.add r 'R'
    |> Map.add s 'S'
    |> Map.add u 'U'
    |> Map.add y 'Y'
    |> Map.add z 'Z'

let toString (glyph:T) = glyphs |> Map.tryFind glyph |> Option.map string

let translate (pixels : Pixel list list) =
    let glyphWidth = 5
    let glyphHeight = 6

    let rendered = pixels |> List.map (fun row -> row |> List.map Pixel.render)

    let height = rendered |> List.length
    if height <> glyphHeight then failwithf "Invalid height for a set of glyphs. Expected height %i; received height %i" glyphHeight height

    let firstRow = rendered |> List.head
    let width = firstRow |> List.length
    if width % 5 <> 0 then failwithf "Invalid width for a set of glyphs. Expected width in a multiple of %i; received width %i" glyphWidth width

    let numChars = width / glyphWidth

    [0..numChars - 1]
    |> List.map (fun charIndex ->
        let minIndex = charIndex * glyphWidth
        let maxIndex = charIndex * (glyphWidth + 1) - 1

        let thisGlyphData =
            rendered
            |> List.map (fun row ->
                row.[minIndex..maxIndex]
                |> List.map string
                |> String.concat ""
                )

        match toString thisGlyphData with
        | Some s -> s
        | None -> "?"

        )
    |> String.concat ""