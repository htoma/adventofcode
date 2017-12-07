open System
open System.Text.RegularExpressions

let getMolecule (line: string) = 
    let res = Regex.Match(line, @"(\w+) => (\w+)")
    res.Groups.[1].Value, res.Groups.[2].Value

let readMolecules = 
    let content = IO.File.ReadAllLines(@"c:\temp\19m.txt")
    content
    |> List.ofArray |> List.map (fun v -> v|> getMolecule)

let text = IO.File.ReadAllText(@"c:\temp\19.txt")

let generate (k:string,v:string) (line: string) = 
    let res = Regex.Matches(line, k)
    [for g in res -> 
        line.Substring(0, g.Index) + v + line.Substring(g.Index+k.Length)]

readMolecules 
|> List.map (fun (k,v) -> generate (k,v) text)
|> List.concat
|> List.distinct
|> List.length