module KnotHashes

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

    let inline xor a b = 
        a ^^^ b

    let padHex v =
        let res = sprintf "%0x" v
        if res.Length = 2 then res else sprintf "0%s" res


    let giveMeKnot (key: string) = 
        let lengths = List.concat [ key
                                    |> explode
                                    |> List.map (int)
                                    [17;31;73;47;23] ]

        let moreLengths = [for _ in 0..63 -> lengths]
                          |> List.concat

        let numbers = [for i in 0..255 -> i]

        let sparseHash = moreLengths 
                         |> List.fold folder {CurPos = 0; Skip = 0; Numbers = numbers}

        let denseHash = sparseHash.Numbers
                         |> List.splitInto 16
                         |> List.map (fun l -> l |> List.fold xor 0)
                         |> List.map padHex

        String.Join("", denseHash)