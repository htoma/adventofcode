#load "../Strings.fsx"

open System
open Strings

let content = IO.File.ReadAllLines(@"2017\data\Advent1-2.txt").[0] 
                |> explode
let step = content.Length / 2

let digits = List.take step content
             |> List.append content
             |> Array.ofList

digits 
|> Array.windowed (step + 1)
|> Array.sumBy (fun a -> if a.[0] = a.[step] then a.[0] |> charToInt else 0)