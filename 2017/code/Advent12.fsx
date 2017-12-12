#load "../Strings.fsx"

open System
open System.Text.RegularExpressions

let parseLine (line: string) =
    let m = Regex.Match(line, @"^(\w+) <-> ([\w, ]+)")
    m.Groups.[1].Value, m.Groups.[2].Value.Split([|" ";","|], StringSplitOptions.RemoveEmptyEntries) |> List.ofArray

let edges = IO.File.ReadAllLines(@"2017\data\Advent12.txt")
             |> Array.map parseLine
             |> dict

let rec count (el: string) (found: string list) =
    let vertices = edges.[el] |> List.filter (fun v -> found |> List.contains v |> not)
    match vertices with
    | [] -> found
    | l -> let newFound = List.concat [found; vertices] 
           (List.collect (fun v -> count v newFound) l) |> List.distinct

let res = count "0" ["0"] |> List.distinct
res.Length