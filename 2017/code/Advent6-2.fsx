#load "../Strings.fsx"

open System
open Strings

let buildMemory (values: int array) =
    String.Join("-", values)

let rec distribute (values: int array) (memories: (string*int) list) count =
    let hash = values |> buildMemory

    match List.tryFind (fun (h, _) -> h = hash) memories with
    | Some (_, i) -> count - i
    | None ->
        // find the largest block of memory
        // if more than one, smallest index wins
        let sorted = values 
                    |> Array.mapi (fun i v -> i, v)
                    |> Array.sortByDescending (fun (i, v) -> v, -i)
        let (index, value) = sorted.[0]
        let newValues = values
                        |> Array.mapi (fun i v ->
                                        match i, v with
                                        | k, _ when k = index -> value / values.Length
                                        | _ -> 
                                                let updatedIndex = if i < index then i + values.Length else i
                                                if updatedIndex - index <= value % values.Length then v + value / values.Length + Math.Min(1, value % values.Length)
                                                else v + value / values.Length
                                       )
        distribute newValues ((hash, count) :: memories) (count + 1)

let line = IO.File.ReadAllLines(@"2017\data\Advent6-2.txt").[0]
distribute (line.Split([|"\t"|], StringSplitOptions.RemoveEmptyEntries) |> Array.map toNumber) [] 0
