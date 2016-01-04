open System

let content = IO.File.ReadAllLines(@"c:\temp\8.txt")

let mutable distances = List.empty

let processLine (l:String) =
    let splitted = l.Split('=')
    let distance = splitted.[1].TrimStart() |> Int32.Parse
    let towns = splitted.[0].TrimEnd().Split([|"to"|], StringSplitOptions.None)
    let start = towns.[0].TrimEnd()
    let finish = towns.[1].TrimStart()
    distances <- (start, finish, distance) :: distances
    distances <- (finish, start, distance) :: distances

content
|> Array.map (fun el -> el |> processLine)

let check (s,f) (cities:string list) = 
        (s=cities.Head && not(List.exists ((=) f) cities)) ||
        (f=cities.Head && not(List.exists ((=) s) cities))

let mutable solutions = List.empty

let rec compute (distances: (string*string*int) list) (cities: string list) length =
    let endings = 
                distances
                |> List.filter (fun (s,f,v) -> check (s,f) cities)
                |> List.map (fun (s,f,v) -> 
                        compute (List.except [s,f,v] distances) (if s=cities.Head then f::cities else s::cities) (length+v))
            
    match endings with
    | [] -> 
//            cities
//            |> List.iter (fun v -> printf "%s->" v)
//            printfn "%i" length
            solutions <- (length, cities) :: solutions
            length
    | _ -> endings |> List.max

distances
|> List.map (fun (s,f,v) -> 
                    compute (List.except [s,f,v] distances) (f::[s]) v)
|> List.max

solutions
|> List.sortBy fst
|> List.last

