open System
let rec remove i l =
    match l with
    | [] -> []
    | [i] -> []
    | p::tail when p = i -> tail    
    | head::tail -> head::remove i tail

type Pipe = 
    { Start: int
      End: int }      
let rec connect (pipes: Pipe list) endV (total: int) = 
    match pipes |> List.filter (fun x -> x.Start = endV || x.End = endV) with
    | [] -> total
    | l -> let comp x = if x.Start = endV then x.End else x.Start                      
           l
           |> List.map (fun x -> connect (pipes |> remove x) (x |> comp) (total + x.Start + x.End))
           |> List.max

let rec bridge (pipes: Pipe list) endV longest total = 
    match pipes |> List.filter (fun x -> x.Start = endV || x.End = endV) with
    | [] -> longest, total
    | l -> let comp x = if x.Start = endV then x.End else x.Start                      
           l 
           |> List.map (fun x -> bridge (pipes |> remove x) (x |> comp) (longest + 1) (total + x.Start + x.End))
           |> List.max
           
let parseLine (line: string) =
    let v = line.Split('/')
    {Start = v.[0] |> Int32.Parse; End = v.[1] |> Int32.Parse }
let pipes = IO.File.ReadAllLines(@"2017\data\Advent24.txt")
            |> Array.map parseLine
            |> List.ofArray

let part1 = pipes
             |> List.filter (fun x -> x.Start = 0)
             |> List.map (fun x -> connect (pipes |> remove x) x.End x.End)
             |> List.max
             
let part2 = pipes
            |> List.filter (fun x -> x.Start = 0)
            |> List.map (fun x -> bridge (pipes |> remove x) x.End 1 x.End)
            |> List.max
            