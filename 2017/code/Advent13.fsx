open System
open System.Text.RegularExpressions

type Firewall = 
    { Depth: int
      Range: int
      Pos: int }

let advanceScanner (values: Firewall list) =
    values |> List.map (fun f -> {f with Pos = f.Pos + 1}) // increment and use module to compute if caught

let rec walk (pos: int) (values: Firewall list) (severity: int) =
    match values with
    | [] -> severity
    | head::tail -> 
                    match head.Depth, head.Pos with
                    | d, _ when d > pos -> walk (pos + 1) (values |> advanceScanner) severity
                    | d, p when d = pos && p % (2 * (head.Range - 1)) <> 0 -> walk (pos + 1) (tail |> advanceScanner) severity
                    | d, p when d = pos && p % (2 * (head.Range - 1)) = 0 ->
                              walk (pos + 1) (tail |> advanceScanner) (severity + 1 + d * head.Range) 
                    | _ -> failwith "Invalid movement"

let rec advanceWithStep step (values: Firewall list) =
    if step = 0 then values
    else 
        advanceWithStep (step - 1) (values |> advanceScanner)

let rec findSneakyStep step max (values: Firewall list) =
    if step = max + 1 then printfn "Could not sneak after max wait %i" max
    else
        let caught  = walk 0 (advanceWithStep step values) 0
        if caught = 0  then printfn "Could sneak while waiting %i" step
        else findSneakyStep (step + 1) max values

let parseLine (line: string) =
    let f = Regex.Match(line, @"^(\d+): (\d+)")
    { Depth = Int32.Parse(f.Groups.[1].Value)
      Range = Int32.Parse(f.Groups.[2].Value)
      Pos = 0 }

let values = IO.File.ReadAllLines(@"2017\data\Advent13.txt")
             |> Array.map parseLine
             |> List.ofArray
            
let series = values |> List.map (fun f -> (
                                            fun x -> if f.Depth = 0 then 
                                                        2 * (f.Range - 1) * x
                                                     else 
                                                        2 * (f.Range - 1) - f.Depth + 2 * (f.Range - 1) * x))

//walk 0 values 0
//findSneakyStep 0 10 values

let value = 10000000
let generated = [0..value]
                |> List.collect (fun v -> series |> List.map (fun f -> f(v)))
                |> List.sortBy id
                |> List.distinct
                |> Set.ofList

[0..value]
|> List.tryFind (fun v -> v |> generated.Contains |> not)