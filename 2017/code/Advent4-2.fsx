#load "../Strings.fsx"

open System
open Strings

let wordHash (value: string) =
    let chars = value 
                |> explode
                |> List.groupBy id
                |> List.sortBy (fun (c, _) -> c)
                |> List.map (fun (c, g) -> sprintf "%c%i" c (g |> List.length))
    String.Join(":", chars |> Array.ofList)

let hasDuplicate (line: string) =
    let head = line.Split([|" "|], StringSplitOptions.RemoveEmptyEntries)
                |> Array.groupBy wordHash
                |> Array.map (fun (_, w) -> w |> Array.length)
                |> Array.sortByDescending id
                |> Array.head
    head  = 1

IO.File.ReadAllLines(@"2017\data\Advent4-2.txt")
|> Array.map hasDuplicate
|> Array.filter id
|> Array.length

