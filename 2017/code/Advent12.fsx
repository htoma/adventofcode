#load "../Strings.fsx"

open System
open System.Text.RegularExpressions

let parseLine (line: string) =
    let m = Regex.Match(line, @"^(\w+) <-> ([\w, ]+)")
    m.Groups.[1].Value, m.Groups.[2].Value.Split([|" ";","|], StringSplitOptions.RemoveEmptyEntries) |> List.ofArray

let edges = IO.File.ReadAllLines(@"2017\data\Advent12.txt")
             |> Array.map parseLine
             |> List.ofArray

let edgeDict = edges |> dict

let rec getGroup (el: string) (found: string list) =
    let vertices = edgeDict.[el] |> List.filter (fun v -> found |> List.contains v |> not)
    match vertices with
    | [] -> found
    | l -> let newFound = List.concat [found; vertices] 
           (List.collect (fun v -> getGroup v newFound) l) |> List.distinct

let rec countGroups (edges: (string * string list) list) count =
    match edges with
    | [] -> count
    | (v, _)::_ -> let group = getGroup v [v]
                   let diff = edges |> List.filter (fun (v, _) -> group |> List.contains v |> not)
                   countGroups diff (count + 1)

countGroups edges 0
