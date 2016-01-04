open System
open System.Text.RegularExpressions

let checkValues =
    [("children", 3); 
        ("cats", 7);
        ("samoyeds", 2);
        ("pomeranians", 3);
        ("akitas", 0);
        ("vizslas", 0);
        ("goldfish", 5);
        ("trees", 3);
        ("cars", 2);
        ("perfumes", 1)] |> dict

let readRelation (line: String) =
    let m = Regex.Match(line, @"(\w+): capacity ([-0-9]+), durability ([-0-9]+), flavor ([-0-9]+), texture ([-0-9]+), calories ([-0-9]+)")
    [|m.Groups.[2].Value |> Int32.Parse; m.Groups.[3].Value |> Int32.Parse; m.Groups.[4].Value |> Int32.Parse; m.Groups.[5].Value |> Int32.Parse; m.Groups.[6].Value |> Int32.Parse|]


let compute line = 
    let id = Regex.Match(line, @"Sue (\d+):").Groups.[1].Value |> Int32.Parse
    let values = Regex.Matches(line, @"(\w+: \d+)")
                    |> Seq.cast<Match>
                    |> Seq.map (fun v -> v.Groups.[1].Value)
                    |> Seq.map (fun v -> 
                                    let tmp = v.Split(':')
                                    tmp.[0], tmp.[1] |> Int32.Parse)
                    |> List.ofSeq
    id,values

let check data =
    data
    |> List.forall (fun (k,v) -> 
        match k with
        | "cats" | "trees" -> checkValues.[k]<v
        | "pomeranians" | "goldfish" -> checkValues.[k]>v
        | _ -> checkValues.[k]=v)

let content = IO.File.ReadAllLines(@"c:\temp\16.txt")

content
|> Array.map (fun l -> l |> compute)
|> Array.filter (fun (id,data) -> data |> check)
