// Day 8: Space Image Format
// https://adventofcode.com/2019/day/8

#r "bin/Debug/netcoreapp3.1/AdventOfCode2019.dll"

type Row = int list
type Layer = Row list
type Image = Layer list

//type Pixel = Black | White | Transparent
//module Pixel =
//    let fromInt i =
//        if i = 0 then Black
//            else if i = 1 then White
//            else Transparent

//    let toInt p =
//        match p with
//        | Black -> 0
//        | White -> 1
//        | Transparent -> 2
//    let render p =
//        match p with
//        | Black -> '#'
//        | White -> '.'
//        | Transparent -> ' '

//let TRANSPARENT = 2

//type Glyph = string list
//module Glyph =

//    let glyphs =
//        let a = [
//            ".##.."
//            "#..#."
//            "#..#."
//            "####."
//            "#..#."
//            "#..#."
//        ]

//        let b = [
//            "###.."
//            "#..#."
//            "###.."
//            "#..#."
//            "#..#."
//            "###.."
//        ]

//        let c = [
//            ".##.."
//            "#..#."
//            "#...."
//            "#...."
//            "#..#."
//            ".##.."
//        ]

//        let e = [
//            "####."
//            "#...."
//            "###.."
//            "#...."
//            "#...."
//            "####."
//        ]

//        let f = [
//            "####."
//            "#...."
//            "###.."
//            "#...."
//            "#...."
//            "#...."
//        ]

//        let g = [
//            ".##.."
//            "#..#."
//            "#...."
//            "#.##."
//            "#..#."
//            ".###."
//        ]

//        let h = [
//            "#..#."
//            "#..#."
//            "####."
//            "#..#."
//            "#..#."
//            "#..#."
//        ]

//        let j = [
//            "..##."
//            "...#."
//            "...#."
//            "...#."
//            "#..#."
//            ".##.."
//        ]

//        let k = [
//            "#..#."
//            "#.#.."
//            "##..."
//            "#.#.."
//            "#.#.."
//            "#..#."
//        ]

//        let l = [
//            "#...."
//            "#...."
//            "#...."
//            "#...."
//            "#...."
//            "####."
//        ]

//        Map.empty
//        |> Map.add a "A"

//    let toString (glyph:Glyph) = glyphs |> Map.tryFind glyph

module Row =
    let fromString (str:string) : Row =
        str.ToCharArray()
        |> Array.map (fun c -> c |> string |> int)
        |> Array.toList

    let toString (row:Row) = row |> List.map string |> String.concat ""

    let empty width : Row = [ for i in 0..width - 1 -> Glyph.Pixel.Transparent |> Glyph.Pixel.toInt]

    let width (row:Row) = row |> List.length

module Layer =
    let fromString (width, height) (str:string) : Layer =
        [0..height - 1]
        |> List.map (fun rowIndex ->
            let startIndex = rowIndex * width
            let thisRow = str.Substring(startIndex, width)

            //printfn "\tRow %i: %s" rowIndex thisRow

            thisRow
            |> Row.fromString
            )

    let toString (layer:Layer) = layer |> List.map Row.toString |> String.concat ""

    let empty width height : Layer = [0..height - 1] |> List.map (fun _ -> Row.empty width)

    let width (layer:Layer) = layer |> List.head |> Row.width
    let height (layer:Layer) = layer |> List.length

    let countDigit (digit:int) (layer:Layer) =
        // List.sumBy is basically List.map |> List.sum
        layer
        |> List.sumBy (fun row ->
            row
            |> List.countBy (fun x -> x = digit)
            |> List.tryFind (fun (x,_) -> x)
            |> Option.map snd
            |> Option.defaultValue 0
            )

module Image =
    let fromString (width, height) (str:string) : Image =
        let s = str.Replace(" ", "")

        let pixelsPerLayer = width * height
        if s.Length % pixelsPerLayer <> 0 then
            failwithf "The provided string does not match the provided dimensions. Expected a string with length divisible by %i, but got a string of length %i"
                pixelsPerLayer
                s.Length

        let layerCount = s.Length / pixelsPerLayer
        [0..layerCount - 1]
        |> List.map (fun layerIndex ->
            let thisStartIndex = layerIndex * pixelsPerLayer
            let thisLayer = s.Substring(thisStartIndex, pixelsPerLayer)

            //printfn "Layer %i: %s" layerIndex thisLayer

            thisLayer
            |> Layer.fromString (width, height)
            )

    let toString (image:Image) = image |> List.map Layer.toString |> String.concat ""

    let width (image:Image) = image |> List.head |> Layer.width
    let height (image:Image) = image |> List.head |> Layer.height
    let layerCount (image:Image) = image |> List.length

    let layerWithLeast (digit:int) (image:Image) : Layer =
        image
        |> List.map (fun layer -> layer, (Layer.countDigit digit layer))
        |> List.minBy (fun (layer, count) -> count)
        |> fst

    let layerWithMost (digit:int) (image:Image) : Layer =
        image
        |> List.map (fun layer -> layer, (Layer.countDigit digit layer))
        |> List.maxBy (fun (layer, count) -> count)
        |> fst

    let layerIndexWithMost digit image =
        let layer = layerWithMost digit image
        image |> List.findIndex (fun x -> x = layer)

    ///
    /// Flattens the image into a single layer by determining which pixels are
    /// opaque and which are transparent.
    /// This is also the "decoding" function for part 2.
    ///
    let flatten (image:Image) : Layer =
        let width = image |> width
        let height = image |> height

        // Helper function to select which pixel value takes priority.
        // If the previous pixel is transparent (2) then use the new one,
        // otherwise, use the existing one.
        let pickPixel x y = if x = 2 then y else x

        let updateRow (existingRow:Row) (newRow:Row) : Row =
            let w = existingRow |> Row.width
            [0..w - 1]
            |> List.map (fun i ->
                let x = existingRow.[i]
                let y = newRow.[i]
                pickPixel x y
                )

        let updateLayer (existingLayer:Layer) (newLayer:Layer) : Layer =
            let h = existingLayer |> Layer.height
            [0..h - 1]
            |> List.map (fun i ->
                let x = existingLayer.[i]
                let y = newLayer.[i]
                updateRow x y
                )

        let folder (resultSoFar:Layer) (currentLayer:Layer) = updateLayer resultSoFar currentLayer
        let initialState = Layer.empty width height

        image |> List.fold folder initialState

    let render (image:Image) =
        let layer = flatten image
        let pixels = layer |> List.map (fun row -> row |> List.map Glyph.Pixel.fromInt)

        pixels
        |> List.map (fun row ->
            row
            |> List.map (fun i ->
                printf "%c" (Glyph.Pixel.render i)
            )
            |> ignore

            printfn ""
            )
        |> ignore

        let translated = Glyph.translate pixels
        printfn "Translated:\n%s" translated

        translated

        //let visible = flatten image

        //for row in visible do
        //    for col in row do
        //        let char =
        //            match col with
        //            | 0 -> '#'
        //            | 1 -> ' '
        //            | _ -> ' '

        //        printf "%c" char

        //    printfn ""


module Part1 =
    let check debug =

        let image = Image.fromString (3, 2) "123356789012"

        Testing.check (Layer.countDigit 3) debug (image.[0], 2)
        Testing.check (Image.layerIndexWithMost 3) debug (image, 0)

    let solve () =
        let image = PuzzleInput.read 8 |> Image.fromString (25, 6)
        let layer = image |> Image.layerWithLeast 0

        let countOnes = Layer.countDigit 1 layer
        let countTwos = Layer.countDigit 2 layer
        let answer = countOnes * countTwos

        printfn "\nPart 1 solution: %i\n" answer

module Part2 =
    let check debug =
        let image = Image.fromString (2,2) "0222112222120000"
        let expected = Layer.fromString (2,2) "0110"

        let image2 = Image.fromString (4,4) "00002222111122221111111111110000"
        let expected2 = Layer.fromString (4,4) "0000111111110000"

        let image3 = Image.fromString (2,2) "2222 2121 2222 0202 2222 0000"
        let expected3 = Layer.fromString (2,2) "0101"

        Testing.check (Image.flatten) debug (image, expected)
        Testing.check (Image.flatten) debug (image2, expected2)
        Testing.check (Image.flatten) debug (image3, expected3)

    let solve () =
        let image = PuzzleInput.read 8 |> Image.fromString (25, 6)
        image |> Image.render

        //let answer = image |> Image.flatten |> Layer.toString

        //printfn "\nPart 2 image definition: %\n" answer

//Part1.check false
//Part1.solve()

Part2.check false
Part2.solve()
