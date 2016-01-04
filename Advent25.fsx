open System

let mult = 252533L
let rest = 33554393L

let getResult (value: int64) =
    let res = value * mult
    res % rest

let getNextPos (i: int, j: int) =
    match (i,j) with
    | (1,_) -> (j+1,1)
    | _ -> (i-1,j+1) 

let rec compute (value: int64) (i: int, j:int) (xLimit: int, yLimit: int) =
    match (i,j) with
    | (x,y) when x=xLimit && y=yLimit -> value
    | _ -> compute (getResult value) (getNextPos (i,j)) (xLimit,yLimit)

compute 20151125L (1,1) (2947,3029)








