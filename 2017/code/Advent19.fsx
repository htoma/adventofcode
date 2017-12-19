open System

let inline explode (s: string) = [|for c in s -> c|]

type Direction = 
    | Down
    | Right
    | Up
    | Left

type Matrix = 
    { values: char[][]
      n: int
      m: int }

let getNewDirection (map: Matrix) posi posj (direction: Direction) =
    let (pi, pj) = match direction with 
                     | Down -> posi - 1, posj
                     | Right -> posi, posj - 1
                     | Up -> posi + 1, posj
                     | Left -> posi, posj + 1
    let (ni, nj) = [posi + 1, posj; posi, posj + 1; posi - 1, posj; posi, posj - 1]
                   |> List.filter (fun (i, j) -> i >= 0 && i < map.n && j >= 0 && j < map.m && map.values.[i].[j] <> ' ')
                   |> List.find (fun (i, j) -> i <> pi && j <> pj)
    if ni = posi + 1 then Direction.Down, ni, nj
    elif nj = posj + 1 then Direction.Right, ni, nj
    elif ni = posi - 1 then Direction.Up, ni, nj
    elif nj = posj - 1 then Direction.Left, ni, nj
    else failwith "Invalid direction"

let isEndpoint (map: Matrix) posi posj (direction: Direction) =
        match direction with
        | Direction.Down -> posi = map.n - 1 || map.values.[posi + 1].[posj] = ' '
        | Direction.Right -> posj = map.m - 1 || map.values.[posi].[posj + 1] = ' '
        | Direction.Up -> posi = 0 || map.values.[posi - 1].[posj] = ' '
        | Direction.Left -> posj = 0 || map.values.[posi].[posj - 1] = ' '

let getOn posi posj (direction: Direction) =
        match direction with
        | Direction.Down -> posi + 1, posj
        | Direction.Right -> posi, posj + 1
        | Direction.Up -> posi - 1, posj
        | Direction.Left -> posi, posj - 1

let rec drive (map: Matrix) posi posj (direction: Direction) (letters: char list) =
    match map.values.[posi].[posj] with
    | '|' | '-' -> 
                    let (newPosi, newPosj) = getOn posi posj direction
                    drive map newPosi newPosj direction letters
    | '+' -> // change direction
             let (newDirection, newPosi, newPosj) = getNewDirection map posi posj direction
             drive map newPosi newPosj newDirection letters
    | c when c >= 'A' && c <= 'Z' -> let newLetters = List.append letters [c]
                                     if isEndpoint map posi posj direction then newLetters
                                     else 
                                         let (newPosi, newPosj) = getOn posi posj direction
                                         drive map newPosi newPosj direction newLetters
    | _ -> failwith "Middle of nowhere"
                 
let map = IO.File.ReadAllLines(@"2017\data\Advent19.txt")
          |> Array.map explode

let posj = map.[0] |> Array.findIndex (fun x -> x = '|')
let result = drive {values = map; n = map.Length; m = map.[0].Length} 0 posj Direction.Down []
String.Join("", result)
