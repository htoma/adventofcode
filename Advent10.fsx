open System

let content = "1113222113"

let compute (number: string) (n: int) = 
    let rec walk (l: int list) count = 
        match count with
        | 0 -> l
        | _ ->
            let stopWatch = System.Diagnostics.Stopwatch.StartNew()
            let res = l |> List.fold (fun state v -> 
                                            match state with
                                            | [] -> v::[1]
                                            | x::n::tail when x=v -> x::(n+1)::tail
                                            | _ ->   v::1::state) []
            stopWatch.Stop()
            printfn "Elapsed at iteration %i with length %i: %A" count res.Length stopWatch.ElapsedMilliseconds
            walk (res |> List.rev) (count-1)
    let res = walk (number |> List.ofSeq |> List.map (fun v -> (int v) - (int '0'))) n
    String.Join("", res)

(compute content 40).Length
