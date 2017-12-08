#load "../Strings.fsx"

open System
open Strings

let steps = IO.File.ReadAllLines(@"2017\data\Advent5-2.txt")
            |> Array.map toNumber

let rec escape (values: int array) pos count =
    let decinc v =
        if v >= 3 then v - 1
        else v + 1
    if pos < 0 || pos >= (values |> Array.length) then count
    else
        let newPos = pos + values.[pos]
        let newValues = if pos > 0 then Array.concat [values |> Array.take pos; [|values.[pos] |> decinc|]; values |> Array.skip (pos + 1)]
                        else Array.concat [[|values.[pos] |> decinc|]; values |> Array.skip (pos + 1)]
        escape newValues newPos (count + 1)

escape steps 0 0
