#load "../Strings.fsx"

open System
open Strings

type State =
    { CurPos: int
      Skip: int
      Numbers: int list }

let folder (state: State) (length: int) =
    if length > state.Numbers.Length then state
    else
        let takeAfter = Math.Min(length, state.Numbers.Length - state.CurPos)
        let first = if takeAfter = length then 0 else length - takeAfter
        let toRotate = if first = 0 then state.Numbers |> List.skip state.CurPos |> List.take length
                                    else List.concat [state.Numbers |> List.skip state.CurPos |> List.take takeAfter; state.Numbers |> List.take first]
                       |> List.rev
        let firstPart = if first > 0 then toRotate |> List.skip takeAfter else []
        let secondPart = if state.CurPos = 0 then []
                         else
                            if firstPart.Length = state.CurPos then []
                            else state.Numbers |> List.skip firstPart.Length |> List.take (state.CurPos - firstPart.Length)
        let thirdPart = toRotate |> List.take takeAfter
        let fourthPart = if state.CurPos + takeAfter >= state.Numbers.Length then []
                         else state.Numbers |> List.skip (state.CurPos + takeAfter)
        let newNumbers = List.concat [firstPart; secondPart; thirdPart; fourthPart]
        { CurPos = (state.CurPos + state.Skip + length) % state.Numbers.Length
          Skip = state.Skip + 1 
          Numbers = newNumbers}

let lengths = List.concat [ IO.File.ReadAllText(@"2017\data\Advent10-2.txt")
                            |> explode
                            |> List.map (int)
                            [17;31;73;47;23] ]

let moreLengths = [for _ in 0..63 -> lengths]
                  |> List.concat

let numbers = [for i in 0..255 -> i]

let sparseHash = moreLengths 
                 |> List.fold folder {CurPos = 0; Skip = 0; Numbers = numbers}

let inline xor a b = 
    a ^^^ b

let denseHash = sparseHash.Numbers
             |> List.splitInto 16
             |> List.map (fun l -> l |> List.fold xor 0)
             |> List.map (fun n -> sprintf "%0x" n)

String.Join("", denseHash)