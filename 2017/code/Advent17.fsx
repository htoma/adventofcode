open System

let append (l: int list) pos value =
    if pos = 0 then List.concat [[l.Head]; [value]; l |> List.skip 1]
    elif pos = l.Length - 1 then List.append l [value]
    else List.concat [l |> List.take (pos + 1); [value]; l |> List.skip (pos + 1)]

let rec move (l: int list) pos cycle step max =
    let actual = cycle % l.Length
    
    let afterPos = if pos + actual < l.Length then pos + actual
                   else actual - (l.Length - pos)

    if step > max then
        l
    else
        let newPos = if afterPos < l.Length then afterPos + 1 else 0
        move (append l afterPos step) newPos cycle (step + 1) max
    
let rec numbers pos cycle step max value =
    if step = max then value
    else
        let newPos = (pos + cycle) % (step + 1) + 1
        numbers newPos cycle (step + 1) max (if pos = 1 then step else value) 

//move [0] 0 355 1 100000
numbers 0 355 0 50000000 0