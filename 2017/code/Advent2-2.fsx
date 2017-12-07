#load "../Strings.fsx"

open System
open Strings

let content = IO.File.ReadAllLines(@"2017\data\Advent2-2.txt")

let number s =
    Int32.Parse s

let findMultiple (values: int array) = 
    let rec divide (a: int array) pos1 pos2 =
        if pos1 >= a.Length then failwith "Impossible"
        else 
            if pos2 >= a.Length then (divide a (pos1 + 1) (pos1 + 2))
            else if a.[pos1] < 2 * a.[pos2] then (divide a pos1 (pos2 + 1))
                else
                    if a.[pos1] % a.[pos2] = 0 then (a.[pos1] / a.[pos2]) 
                    else divide a pos1 (pos2 + 1)
    divide (values |> Array.sortDescending) 0 1
let parseLine (line: string) =
    let values = line.Split([|"\t"|], StringSplitOptions.RemoveEmptyEntries) |> Array.map number
    values |> findMultiple
    

content
|> Array.sumBy parseLine
