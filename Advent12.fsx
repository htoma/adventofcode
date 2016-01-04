open System
open System.Text.RegularExpressions


let content = IO.File.ReadAllText(@"c:\temp\12.json")

let summe text = 
        Regex.Matches(text, @"([-\d]+)", RegexOptions.Singleline)
        |> Seq.cast<Match>
        |> Seq.sumBy (fun v -> v.Groups.[0].Value |> Int32.Parse)


let all = summe content

let hide = 
    Regex.Matches(content, @"{.*?:\s*""red"".*?}", RegexOptions.Singleline)
    |> Seq.cast<Match>
    |> Seq.sumBy (fun v -> v.Groups.[0].Value |> summe)

all-hide

let findIndexLeft (text: String) pos =
    let rec loop idx acc =
        match idx with
        |x when x <=0 -> 0
        |v -> 
            if text.[v]='{' then 
                if acc=0 then v else loop (v-1) (acc-1)
            else
                if text.[v]='}' then loop (v-1) (acc+1)
                else loop (v-1) acc
    loop (pos-1) 0

let findIndexRight (text: String) pos =
    let rec loop idx acc =
        match idx with
        |x when x >=(text.Length-1) -> text.Length-1
        |v -> 
            if text.[v]='}' then 
                if acc=0 then v else loop (v+1) (acc-1)
            else 
                if text.[v]='{' then loop (v+1) (acc+1)
                else loop (v+1) acc
    loop (pos+1) 0
         

let intervals = 
    Regex.Matches(content, @":\s*""red""")
    |> Seq.cast<Match>
    |> Seq.map (fun m ->
            let left = findIndexLeft content m.Index
            let right = findIndexRight content m.Index
            left, right)
    |> List.ofSeq

let inInterval index (list: (int*int) list) =
    List.exists (fun el -> fst el <= index && snd el >= index) list

Regex.Matches(content, @"([-\d]+)", RegexOptions.Singleline)
|> Seq.cast<Match>
|> Seq.filter (fun v -> not (inInterval v.Index intervals))
|> Seq.sumBy (fun v -> v.Groups.[0].Value |> Int32.Parse)
    
