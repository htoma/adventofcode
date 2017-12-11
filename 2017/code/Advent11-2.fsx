#load "../Strings.fsx"

open System

type State = 
    { x: int
      y: int
      max: int }

type Direction = 
    | Vertical
    | Horizontal

let computeSteps (state: State) =
    match state.x, state.y with
    | 0, v -> v
    | v, 0 -> v / 2
    | x, y -> 
            let direction = if Math.Abs(x) / 2 < Math.Abs(y) then Direction.Horizontal else Direction.Vertical
            match direction with
            | Direction.Vertical -> Math.Abs(y) + (Math.Abs(x) - 2 * Math.Abs(y)) / 2
            | Direction.Horizontal -> Math.Abs(x) / 2 + (Math.Abs(y) - Math.Abs(x) / 2) / 2


let folder (state: State) (direction: string) = 
    let newState = match direction with
                    | "n" -> {state with y = state.y - 2}
                    | "s" -> {state with y = state.y + 2}
                    | "ne" -> {state with x = state.x + 2; y = state.y - 1}
                    | "se" -> {state with x = state.x + 2; y = state.y + 1}
                    | "sw" -> {state with x = state.x - 2; y = state.y + 1}
                    | "nw" -> {state with x = state.x - 2; y = state.y - 1}
                    | _ -> failwith "Unrecognized direction"
    {newState with max = Math.Max(newState.max, newState |> computeSteps)}
    
let directions = IO.File.ReadAllText(@"2017\data\Advent11.txt").Split([|","|], StringSplitOptions.RemoveEmptyEntries) 
                 |> List.ofArray

directions |> List.fold folder {x = 0; y = 0; max = 0}