module Day01

open System.IO

let processFile (filePath : string) =
    seq {
        use fileReader = new StreamReader(filePath)

        while not fileReader.EndOfStream do
            let line = fileReader.ReadLine()
            yield line
    }
    
let countFuel mass = (mass / 3) - 2

let result1 =
    processFile "Input01.txt"
    |> Seq.map(int)
    |> Seq.map(countFuel)
    |> Seq.sum

let countTotalFuel initialMass =
    let rec f current total =
        let x = countFuel current
        match x with
        |x when x <= 0 -> total
        |x -> (total + x) |> f x
    f initialMass 0
    
let result2 =
    processFile "Input01.txt"
    |> Seq.map(int)
    |> Seq.map(countTotalFuel)
    |> Seq.sum
