open System
open System.Collections.Generic
open System.Text.RegularExpressions

let content = IO.File.ReadAllLines(@"c:\temp\71.txt")

let values = new Dictionary<string,string>()

let storeValue (l:String) = 
    let data = l.Replace("->", "-").Split '-'
    let key = data.[1].TrimStart()
    values.Add(key, data.[0])

content
|> Array.map (fun l -> storeValue l)


(*
123 -> x
456 -> y
x AND y -> d
x OR y -> e
x LSHIFT 2 -> f
y RSHIFT 2 -> g
NOT x -> h
NOT y -> i
*)

let f op = 
    match op with
    | "AND" -> (&&&)
    | "OR" -> (|||)
    | "LSHIFT" -> (<<<)
    | "RSHIFT" -> (>>>)
    | _ -> failwith "This is not a valid operation"

let pass v =
    match Int32.TryParse v with
    | true, _ -> v
    | _ -> values.[v]

let printy v spaces =
    for _ in [1..spaces] do printf " "
    printfn "%s" v

let rec parse exp spaces =
    //printy exp spaces
    match Int32.TryParse exp with
    | true, v -> v
    | _ ->
        let ops = exp.Split([|' '|], StringSplitOptions.RemoveEmptyEntries) 
        match ops.Length with
        | 1 -> (parse (pass ops.[0]) (spaces+1))
        | 2 -> ~~~(parse (pass ops.[1]) (spaces+1))
        | 3 -> 
            let result = (f ops.[1]) (parse (pass ops.[0]) (spaces+1)) (parse (pass ops.[2]) (spaces+1))
            //printfn "result is %i" result
            result
        | _ -> failwith "Incorrect nb of parameters"
            

parse values.["a"] 1
