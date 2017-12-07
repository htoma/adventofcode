open System
open System.Text.RegularExpressions

let readRelation (line: String) =
    let m = Regex.Match(line, @"(\w+) can fly (\d+) km/s for (\d+) seconds, but then must rest for (\d+) seconds.")
    m.Groups.[1].Value, m.Groups.[2].Value, m.Groups.[3].Value, m.Groups.[4].Value

//let content = IO.File.ReadAllLines(@"c:\temp\13.txt")
let content = IO.File.ReadAllLines(@"c:\temp\14.txt")

let compute duration (name, speed, flight, pause) =
    let distance = speed * flight
    let cycles = duration / (flight + pause) 
    let remain = Math.Min(flight, (int duration) % (flight + pause)) * speed
    cycles * distance + remain

let seconds = 2503

content
|> Array.map (fun l -> l |> readRelation)
|> Array.map (fun (n,s,f,p) -> (n, (compute seconds (n,s |> Int32.Parse, f |> Int32.Parse, p |> Int32.Parse))))
|> Array.maxBy snd

//(name, speed, flight, pause, distance, points)

let rec givePoints duration (data:(string*int*int*int*int*int) list) = 
    match duration with
    | 0 -> 
        data
        |> List.maxBy (fun (_,_,_,_,_,po) -> po)
    | v ->
        let distanceData = 
                data
                |> List.map (fun (n,s,f,pa,_,po) -> (n,s,f,pa,(compute v (n,s,f,pa)),po))
        distanceData |> List.iter (fun (n,s,f,pa,d,po) -> printfn "%s: %i" n d)
        let (winner,_,_,_,_,_) = distanceData |> List.maxBy (fun (_,_,_,_,d,_) -> d)
        printfn "winner after %i seconds: %s" v winner
        givePoints (v-1) (distanceData
                            |> List.map (fun (n,s,f,pa,d,po) ->
                                                if n=winner then (n,s,f,pa,d,po+1) else (n,s,f,pa,d,po)))

givePoints seconds (content
                    |> Array.map (fun l -> l |> readRelation)
                    |> List.ofArray
                    |> List.map (fun (n,s,f,pa) -> (n,s|>Int32.Parse,f|>Int32.Parse,pa|>Int32.Parse,0,0)))