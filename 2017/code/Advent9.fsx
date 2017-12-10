#load "../Strings.fsx"

open System
open Strings

type State =
    { Sum: int
      Level: int
      InTrash: bool
      ShouldCancel: bool }

let folder (state: State) (c: char) =
    match state.InTrash with
    | true -> match state.ShouldCancel with
              | true -> {state with ShouldCancel = false}
              | false -> match c with
                         | '!' -> {state with ShouldCancel = true}
                         | '>' -> {state with InTrash = false}
                         | _ -> state
    | false -> match c with
               | '{' -> {state with Level = state.Level + 1}
               | '}' -> {state with Sum = state.Sum + state.Level; Level = state.Level - 1}
               | '<' -> {state with InTrash = true}
               | _ -> state
let stream = IO.File.ReadAllText(@"2017\data\Advent9.txt")
             |> explode

stream |> List.fold folder { Sum = 0; Level = 0; InTrash = false; ShouldCancel = false}