open System

let password = "vzbxxyzz"

let increasing (word: String) = 
    word 
    |> Seq.windowed 3
    |> Seq.exists (fun v -> 
                match v with
                | [|a;b;c|] -> (int b)=(int a)+1 && (int c)=(int b)+1
                | _ -> failwith "Invalid value")
    
let notContain (word: String) =
    let notChars = ['i';'o';'l']
    word |> Seq.forall (fun v -> not (List.contains v notChars))

let pairs (word: String) =
    word
    |> Seq.pairwise
    |> Seq.filter (fun (a,b) -> a=b)
    |> Seq.groupBy (fun (a,_) -> a)
    |> Seq.length >= 2 

let rec update (password: String) (pos: int option) = 
       let realPos = match pos with
                     | Some p -> p
                     | None -> password.Length - 1
       match password.[realPos] with
       | 'z' -> update (password.[0..realPos-1] + "a" + password.[realPos+1..password.Length-1]) (Some(realPos-1))
       | v -> password.[0..realPos-1] + (char ((int v)+1)).ToString() + password.[realPos+1..password.Length-1]
    
let rec getResult (word: String) = 
    let result = update word None
    if (result |> increasing) && (result |> notContain) && (result |> pairs) then result
    else (getResult result)

getResult password

increasing "vzbxxzaa"