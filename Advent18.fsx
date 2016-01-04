open System

let size = 100

let createEmpty =
    [|for i in 0..size+1 -> 
        [|for j in 0..size+1 -> 0|]|]

let processLine i (line: string) (m: int[][]) =
    for j in 0..(line.Length-1) do
        match line.[j] with
        | '#' -> m.[i+1].[j+1] <- 1
        | _ -> m.[i+1].[j+1] <- 0

let content = IO.File.ReadAllLines(@"c:\temp\18.txt")

let m = [|for i in 0..size+1 -> 
            [|for j in 0..size+1 -> 0|]|]

content
|> Array.mapi (fun i v -> processLine i v m)

let printM (matrix:int[][])=
    printfn""
    for i in 1..size do
        for j in 1..size do
            printf "%i " matrix.[i].[j]
        printfn ""

//printM m

let update (matrix: int[][]) =
    let temp = [|for i in 0..size+1 -> 
                     [|for j in 0..size+1 -> 0|]|]
    for i in 1..size do
        for j in 1..size do
            //printM  matrix
            let ngb = [matrix.[i-1].[j-1];matrix.[i-1].[j];matrix.[i-1].[j+1];matrix.[i].[j-1];matrix.[i].[j+1];matrix.[i+1].[j-1];matrix.[i+1].[j];matrix.[i+1].[j+1]]
            let count = ngb |> List.filter (fun v -> v=1) |> List.length
            let value = 
                    match i,j with
                    | x,y when (x=1 || x=size) && (y=1 || y=size) -> 1
                    | _ ->
                            match matrix.[i].[j],count with
                            | 1,2 | 1,3 | 0,3 -> 1
                            | _ -> 0
            //printfn "for (%i,%i) found %i -> %i" i j count value
            
            temp.[i].[j] <- value

//            printfn "result"
//            printM temp
    temp

let rec all steps (matrix:int[][]) =
    match steps with
    | 0 -> matrix
    | v -> all (steps-1) (update matrix)

all 100 m
|> Array.sumBy (fun v -> v |> Array.sumBy (fun x -> x))


 