#load "../Strings.fsx"

open System
open Strings

let content = IO.File.ReadAllLines(@"2017\data\Advent2-1.txt")

let number s =
    Int32.Parse s

let parseLine (line: string) =
    let values = line.Split([|"\t"|], StringSplitOptions.RemoveEmptyEntries) |> Array.map number
    (values |> Array.max) - (values |> Array.min)

content
|> Array.sumBy parseLine
