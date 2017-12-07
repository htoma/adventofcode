open System

let numbers = IO.File.ReadAllLines(@"c:\temp\24.txt") |> Array.map Int32.Parse |> List.ofArray

let limit = (numbers |> List.sum) / 4

let mutable minCurr = 30

let diff (numbers: int list) (current: int list) = 
    (Set.ofList numbers - Set.ofList current) |> Set.toList

let rec checkPartition (all: int list) (current: int list) (level: int) = 
    if current |> List.sum > limit then false
    elif current |> List.sum = limit then 
        if  level=0 then true
        else 
            checkPartition (diff all current) [] (level-1)
    else
       all
        |> List.mapi (fun i v -> checkPartition (all |> List.skip (i+1)) (v::current) level)
        |> List.exists (fun s -> s)



let rec compute (all: int list) (current: int list) = 
    if current |> List.sum = limit then 
        if current.Length < minCurr && (checkPartition (diff numbers current) [] 1) then
           minCurr <- current.Length
           [current]
        else  []
    elif current.Length=minCurr || (current |> List.sum > limit) then []
    else 
        all
        |> List.mapi (fun i v -> compute (all |> List.skip (i+1)) (v::current))
        |> List.concat

let solutions = compute numbers []

let minLength = solutions |> List.map (fun l -> l.Length) |> List.min 

solutions
|> List.filter (fun l -> l.Length=minLength)
|> List.map (fun l -> l |> List.fold (*) 1)
|> List.min
