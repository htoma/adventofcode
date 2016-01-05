open System

let divs n =
    {2..n/2}
    |> Seq.filter (fun v -> n%v=0 && n/v<=50)
    |> Seq.sum
    |> fun v -> (v+1+n)*11


{800000..900000}
|> Seq.takeWhile (fun v -> (divs v) < 36000000)
|> Seq.last

divs 884520