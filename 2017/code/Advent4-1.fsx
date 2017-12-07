#load "../Strings.fsx"

open System
open Strings

let hasDuplicate (line: string) =
    let head = line.Split([|" "|], StringSplitOptions.RemoveEmptyEntries)
                |> Array.groupBy id
                |> Array.map (fun (_, w) -> w |> Array.length)
                |> Array.sortByDescending id
                |> Array.head
    head  = 1

IO.File.ReadAllLines(@"2017\data\Advent4-1.txt")
|> Array.map hasDuplicate
|> Array.filter id
|> Array.length

