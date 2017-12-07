open System
open System.Text.RegularExpressions

let rec generate n k =
    match (n,k) with
    | (x,y) when x<y -> []
    | (x,1) ->[[x]]
    | _ -> 
          [for i in 1..(n-(k-1)) ->
                            generate (n-i) (k-1)
                            |> List.filter (fun l -> not (l |> List.isEmpty))
                            |> List.map (fun l -> i::l)]
          |> List.concat

//generate 100 4

let content = IO.File.ReadAllLines(@"c:\temp\15.txt")

let readRelation (line: String) =
    let m = Regex.Match(line, @"(\w+): capacity ([-0-9]+), durability ([-0-9]+), flavor ([-0-9]+), texture ([-0-9]+), calories ([-0-9]+)")
    [|m.Groups.[2].Value |> Int32.Parse; m.Groups.[3].Value |> Int32.Parse; m.Groups.[4].Value |> Int32.Parse; m.Groups.[5].Value |> Int32.Parse; m.Groups.[6].Value |> Int32.Parse|]

let data = content
            |> Array.map (fun v -> v |> readRelation)

let valueCount = data.[0].Length

let compute (configuration: int list) = 
    let values = [for i in 0..valueCount-1 ->
                    configuration
                    |> List.mapi (fun j v -> v * data.[j].[i])
                    |> List.sum
                    |> fun v -> Math.Max(0, v)]
    match values with 
    | [c;d;f;t;500] -> c*d*f*t
    | _ -> 0

let total = 100

generate total data.Length
//|> List.take 1
|> List.map (fun l -> l |> compute)
|> List.max

