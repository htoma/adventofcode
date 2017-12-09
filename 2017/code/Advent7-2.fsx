#load "../Strings.fsx"

open System
open Strings
open System.Text.RegularExpressions

type Node = 
    {
        Text: string
        Cost: int
        Children: string list
    }
let parseLine (line: string) =
    let m = Regex.Match(line, @"^(\w+) \((\d+)\)( -> ([\w, ]+))?")
    let text = m.Groups.[1].Value
    let cost = m.Groups.[2].Value |> Int32.Parse
    let children = if m.Groups.Count > 4 then m.Groups.[4].Value.Split([|" ";","|], StringSplitOptions.RemoveEmptyEntries) 
                                              |> List.ofArray
                                         else []
    {Text = text; Cost = cost; Children = children}

let nodes = IO.File.ReadAllLines(@"2017\data\Advent7-2.txt")
             |> List.ofArray
             |> List.map parseLine

let refNodes = nodes
                |> List.map (fun n -> n.Text, n)
                |> dict

// find the root and work from there on
let parents = nodes
              |> List.filter (fun n -> n.Children.IsEmpty |> not)

let refChildren = parents
                    |> List.map (fun n-> n.Children)
                    |> List.concat
                    |> List.distinct

let root = parents
            |> List.find (fun n -> refChildren |> List.contains n.Text |> not)

let rec process (node: Node) =
    match node.Children with
    | [] -> node.Text, node.Cost
    | l -> 
            let childrenCost = l 
                                |> List.map (fun text -> refNodes.[text])
                                |> List.map (process)
                                |> List.groupBy snd
                                |> List.sortBy (fun (_,g) -> g.Length)
            match childrenCost with
            | [(v, g)] -> node.Text, node.Cost + v * g.Length
            | (v1, (n, _)::_)::(v2, _)::_ -> failwith (sprintf "Wrong weight for %s: should be %i" n (refNodes.[n].Cost + v2 - v1))
            

process root