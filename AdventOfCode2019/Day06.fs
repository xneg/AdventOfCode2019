module Day06

open System.IO

let processFile (filePath : string) =
    seq {
        use fileReader = new StreamReader(filePath)

        while not fileReader.EndOfStream do
            let line = fileReader.ReadLine()
            yield line
    }

let parse (str : string) =
    let x = str.Split([|')'|])
    (x.[1], x.[0])

let createPlanetsList = List.map parse
    
let createMap =
    createPlanetsList >> Map.ofList
    
let dependencyMap =
    processFile "Input06.txt"
    |> Seq.toList
    |> createMap

let planetsList =
    processFile "Input06.txt"
    |> Seq.toList
    |> createPlanetsList
    |> List.map fst
   
let findDepsCount map planet  =
    let rec f count planet (map: Map< 'Key, 'Value>) =
        let result = map.TryFind planet
        match result with
        | Some x -> f (count + 1) x map
        | None -> count
    f 0 planet map

planetsList |> List.map (findDepsCount dependencyMap) |> List.sum

let findDepsList map planet  =
    let rec f depsList planet (map: Map< 'Key, 'Value>) =
        let result = map.TryFind planet
        match result with
        | Some x -> f (x::depsList) x map
        | None -> depsList
    f [] planet map

let intersect a b = Map (seq {
    for KeyValue(k, va) in a do
        match Map.tryFind k b with
        | Some vb -> yield k, (va, vb)
        | None    -> () })

let myMap = "YOU" |> findDepsList dependencyMap |> List.map (fun x -> (x, 1)) |> Map.ofList
let santaMap = "SAN" |> findDepsList dependencyMap |> List.map (fun x -> (x, 1)) |> Map.ofList

let sameMap = intersect myMap santaMap

(Map.count myMap) + (Map.count santaMap) - 2 * (Map.count sameMap) 






