#load "../Strings.fsx"

open System
open Strings

let content = IO.File.ReadAllLines(@"2017\data\Advent1-1.txt").[0] 
                |> explode
let digits = List.append content [content.Head]

digits 
|> List.pairwise
|> List.sumBy (fun (x,y) -> if x = y then x |> charToInt else 0)