open System
open System.Text.RegularExpressions

let generate (values: string list) = 
    let rec gen el (res:list<list<string>>) =
       match res with
       | [] -> [[el]]
       | _ ->
           res
           |> List.map (fun x -> 
                                [for i in 0..(x.Length) -> 
                                            x 
                                            |> List.take i
                                            |> List.append [el] 
                                            |> List.append (x |> List.skip i)])
           |> List.concat
    values
    |> List.fold (fun state el -> gen el state) []


//generate ["a";"b";"c"]

let readRelation (line: String) =
    let m = Regex.Match(line, @"^(\w+) would (\w+) (\d+) happiness units by sitting next to (\w+)")
    m.Groups.[1].Value, m.Groups.[2].Value, m.Groups.[3].Value, m.Groups.[4].Value

let content = IO.File.ReadAllLines(@"c:\temp\13.txt")

let cost = 
        content
        |> Array.map (fun l -> readRelation l)
        |> Array.fold (fun state (left, verb, value, right) -> 
                            let value = Int32.Parse value
                            (left, right, value * (if verb="gain" then 1 else -1)) :: state) []

let mutable people = 
        cost
        |> List.map (fun (v,_,_) -> v)
        |> List.distinct

people <- "Me" :: people

let getValue (ll,rr) =
    match (ll,rr) with
    | ("Me",_) | (_,"Me")-> 0
    | (l,r) ->
        cost
        |> List.filter (fun (s,e,v) -> l=s && r=e)
        |> List.map (fun (_,_,v) -> v)
        |> List.exactlyOne

let relations = generate (people |> List.skip 1)
                |> List.map (fun l -> List.append (people.Head :: l) [people.Head])
                |> List.map (fun l -> l |> List.pairwise)
                |> List.map (fun l -> l |> List.fold (fun state (l,r) -> state + (getValue (l,r)) + (getValue (r,l))) 0)
                |> List.max
                
