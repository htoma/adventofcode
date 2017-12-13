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
                              printfn "pos %i, with (%i: %i and pos %i)" pos head.Depth head.Range head.Pos                     
                              walk (pos + 1) (tail |> advanceScanner) (severity + d * head.Range) 
                    | _ -> failwith "Invalid movement"

let parseLine (line: string) =
    let f = Regex.Match(line, @"^(\d+): (\d+)")
    { Depth = Int32.Parse(f.Groups.[1].Value)
      Range = Int32.Parse(f.Groups.[2].Value)
      Pos = 0 }

let values = IO.File.ReadAllLines(@"2017\data\Advent13.txt")
             |> Array.map parseLine
             |> List.ofArray

walk 0 values 0