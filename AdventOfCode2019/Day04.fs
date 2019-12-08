module Day04

let parseNumber n =
    let rec f n result =
        match n with
        | 0 -> result
        | n ->
            let i = n % 10
            let n = (n - i) / 10
            f n (i::result)
    f n []

let checkNumber n =
    let rec f pair ns =
        match ns with
        | [_] | [] -> if pair then Some n else None
        | a::tail ->
            let b = tail |> List.head
            if b < a then None
            elif a = b then f true tail
            else f pair tail
    (parseNumber >> f false) n

let list1 = [265275..781584] |> List.choose checkNumber

list1 |> List.length //960

let checkNumber2 n =
    let rec f clustersCount repeatsCount ns =
        match ns with
        | [_] | [] -> if clustersCount > 0 || repeatsCount = 1 then Some n else None
        | a::tail ->
            let b = tail |> List.head
            if b < a then None
            elif a = b then f clustersCount (repeatsCount + 1) tail
            else f (clustersCount + (if repeatsCount = 1 then 1 else 0)) 0 tail
    (parseNumber >> f 0 0) n
    
let list2 = [265275..781584] |> List.choose checkNumber2

list1 |> List.except list2

list2 |> List.length
    
