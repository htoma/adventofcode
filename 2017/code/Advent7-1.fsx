#load "../Strings.fsx"

open System
open Strings
open System.Text.RegularExpressions

let parseLine (line: string) =
    let m = Regex.Match(line, @"^(\w+) \(\d+\)( -> ([\w, ]+))?")
    m.Groups.[1].Value, if m.Groups.Count > 3 then m.Groups.[3].Value.Split([|" ";","|], StringSplitOptions.RemoveEmptyEntries) 
                                              else [||]

let values = IO.File.ReadAllLines(@"2017\data\Advent7-1.txt")
             |> Array.map parseLine

let roots = values |> Array.map fst
let children = values |> Array.map snd |> Array.concat |> Array.distinct

roots
|> Array.tryFind (fun e -> children |> Array.contains e |> not)
