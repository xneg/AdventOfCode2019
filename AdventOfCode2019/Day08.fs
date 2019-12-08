module Day08

open System.IO

let processFile (filePath : string) =
    use fileReader = new StreamReader(filePath)
    fileReader.ReadLine()

let toIntsList (str : string) =
    let inline charToInt c = int c - int '0'
    str |> Seq.toList |> List.map charToInt

let parseToLayers width height intsList =
    let rec f layers intsList =
        let (newLayer, tail) = intsList |> List.splitAt (width * height)
        let layers = newLayer::layers
        match tail with
        | [] -> layers
        | tail -> tail |> f layers
    f [] intsList

let layers =
    "Input08.txt"
    |> processFile
    |> toIntsList
    |> parseToLayers 25 6

let firstPartSolve =
    let minZeroListIndex =
        List.map (fun l -> l |> List.fold (fun acc e -> if e = 0 then acc + 1 else acc) 0)
        >> List.mapi (fun i v -> i, v)
        >> List.minBy snd
        >> fst
    let solve a = fst a * snd a

    layers.[layers |> minZeroListIndex]
    |> List.fold (
                     fun acc e ->
                         if e = 1 then (fst acc + 1, snd acc)
                         elif e = 2 then (fst acc, snd acc + 1)
                         else acc
                 ) (0, 0)
    |> solve

// 2nd part

let mergeLayers i (layers: int list list) =
    let mergePixels =
        let reduction top bottom = if top = 2 then bottom else top
        List.reduce reduction
    
    layers
    |> List.fold (fun acc e -> e.[i]::acc) []
    |> mergePixels

let mergeImage width height layers =
    let convertToImage i = if i = 1 then '0' else ' '
    
    [0..width * height - 1]
    |> List.map (fun i -> layers |> mergeLayers i)
    |> List.map convertToImage

let drawImage rowLength charsList =
    let rec f charsList =
        let (row, tail) = charsList |> List.splitAt rowLength
        new string [|for c in row -> c|] |> printfn "%s"
        match tail with
        | [] -> ()
        | tail -> tail |> f
    f charsList
    
let solveSecondPart width height =
    processFile
    >> toIntsList
    >> parseToLayers width height
    >> mergeImage width height
    >> drawImage width
    
"Input08.txt" |> solveSecondPart 25 6