open System
open System.Text.RegularExpressions

let split (separator: string) (f: string->'a) (input: string) =
    input.Split([|separator|], StringSplitOptions.RemoveEmptyEntries)
    |> Array.map (fun v -> v |> f)

type Move = 
    | Rotate of int
    | XChange of int*int
    | PChange of string*string 

let (|RegexMatch|_|) pattern input =
   let m = Regex.Match(input,pattern) 
   if (m.Success) then Some m.Groups.[1].Value 
                  else None

let getMove input =
    match input with
    | RegexMatch @"s(\d+)" v -> Rotate (v |> Int32.Parse)
    | RegexMatch @"x(\d+/\d+)" v -> let swap = v |> split "/" (fun x -> x |> Int32.Parse)
                                    XChange (swap.[0], swap.[1])
    | RegexMatch @"p(\w/\w)" v -> let swap = v |> split "/" (id)
                                  PChange (swap.[0], swap.[1])
    | _ -> failwith "Not an ordinary move"

let rotate (l: 'a list) (pos: int) =
    match pos with
    | 0 -> l
    | v when v < l.Length -> List.concat [l |> List.skip (l.Length - pos); l |> List.take (l.Length - pos)]
    | _ -> failwith "Too large of a rotation"

let exchange (input: 'a list) (v1: 'a, v2: 'a) =
    input
    |> List.map (fun v -> match v with
                          | f when f = v1 -> v2
                          | s when s = v2 -> v1
                          | _ -> v)

let exchangePos  (input: 'a list) (l: int, r: int) =
    let first = if l = 0 then input.Head else input |> List.skip l |> List.head
    let second = if r = 0 then input.Head else input |> List.skip r |> List.head
    (first, second) |> exchange  input

let moves = IO.File.ReadAllText(@"2017\data\Advent16.txt").Split([|","|], StringSplitOptions.RemoveEmptyEntries) 
             |> Array.map getMove
             |> List.ofArray

let input = [for i in 'a'..'p' -> string i]

let parser (input: string list) (move: Move) =
    match move with
    | Rotate v -> v |> rotate input
    | XChange (l, r) -> (l, r) |> exchangePos input
    | PChange (v1, v2) -> (v1, v2) |> exchange input
    

String.Join("", moves |> List.fold parser input)
