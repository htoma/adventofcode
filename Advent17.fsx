open System

let rec pay sum (coins: int list) count =
    match coins,sum with
    | _,0 -> [count]
    | _,v when v < 0 -> []
    | [],_ -> []
    | _,_ -> 
        let l = pay (sum-coins.Head) coins.Tail (count+1)
        let r = pay sum coins.Tail count
        List.append l r

let coins = IO.File.ReadAllLines(@"c:\temp\17.txt")
            |> Array.map (fun v -> v |> Int32.Parse)
            |> List.ofArray

pay 150 coins 0
|> List.groupBy (fun v -> v)
|> List.minBy fst
|> fun (_,l) -> l.Length