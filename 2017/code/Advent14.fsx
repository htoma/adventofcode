open System
#load "Advent10-2.fsx"
#load "../Strings.fsx"

let hexes = [( '0', "0000" )
             ( '1', "0001" )
             ( '2', "0010" )
             ( '3', "0011" )
             ( '4', "0100" )
             ( '5', "0101" )
             ( '6', "0110" )
             ( '7', "0111" )
             ( '8', "1000" )
             ( '9', "1001" )
             ( 'a', "1010" )
             ( 'b', "1011" )
             ( 'c', "1100" )
             ( 'd', "1101" )
             ( 'e', "1110" )
             ( 'f', "1111" )]
             |> dict

let countOnes (v: string) =
    v |> Seq.countBy (fun c -> c = '1') |> Seq.sumBy snd

let input = "hxtvlmkl"

let res = [0..127]
            |> List.map (fun v -> sprintf "%s-%i" input v)
            |> List.map (fun v -> KnotHashes.giveMeKnot v)
            |> List.map (fun v -> v |> explode |> List.map (fun c -> hexes.[c]))
            |> List.concat

res |> List.map (fun v -> v |> explode) |> List.concat |> List.countBy (fun v -> v = '1')            

