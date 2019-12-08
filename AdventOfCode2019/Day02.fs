module Day02

let setUpWords noun verb array =
    let tail = array |> Array.skip 3
    Array.concat[|[|array.[0];noun;verb|] ; tail|]

let newArray position value currentArray =
    Array.set currentArray position value
    currentArray

let op opCode a b =
    match opCode with
    | 1 -> a + b
    | 2 -> a * b
    | _ -> 0

let intCode input =
    let rec f input position =
        let instruction = input |> Array.skip position |> Array.take 4
        match instruction with
        | [|opCode;_;_;_|] when opCode = 99 -> input
        | [|opCode;a;b;c|] ->
            let input = input |> newArray c (op opCode input.[a] input.[b])
            f input (position + 4)
        | _ -> input
    (f input 0).[0]

let findWords keyNumber input =
    let rec f input noun verb =
        let output = input |> setUpWords noun verb |> intCode
        match output with
        | x when x = keyNumber -> 100 * noun + verb
        | _ when verb < 99 -> f input noun (verb + 1)
        | _ when verb = 99 -> f input (noun + 1) 0
        | _ -> -1
    f input 0 0

let inputStr = "1,0,0,3,1,1,2,3,1,3,4,3,1,5,0,3,2,1,6,19,1,5,19,23,1,23,6,27,1,5,27,31,1,31,6,35,1,9,35,39,2,10,39,43,1,43,6,47,2,6,47,51,1,5,51,55,1,55,13,59,1,59,10,63,2,10,63,67,1,9,67,71,2,6,71,75,1,5,75,79,2,79,13,83,1,83,5,87,1,87,9,91,1,5,91,95,1,5,95,99,1,99,13,103,1,10,103,107,1,107,9,111,1,6,111,115,2,115,13,119,1,10,119,123,2,123,6,127,1,5,127,131,1,5,131,135,1,135,6,139,2,139,10,143,2,143,9,147,1,147,6,151,1,151,13,155,2,155,9,159,1,6,159,163,1,5,163,167,1,5,167,171,1,10,171,175,1,13,175,179,1,179,2,183,1,9,183,0,99,2,14,0,0"
//let inputStr = "1,9,10,3,2,3,11,0,99,30,40,50"

let input = inputStr.Split ',' |> Array.map(int)
let result1 = input |> setUpWords 12 2 |> intCode
let result2 = input |> findWords 19690720