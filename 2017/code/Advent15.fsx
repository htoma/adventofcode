open System.Diagnostics.Eventing.Reader
#load "../Strings.fsx"

open System

let modulo = 2147483647L
let bitModulo = int64 (Math.Pow(2., 16.))

let genA = 16807L
let genB = 48271L

let startA = 703L
let startB = 516L

let judgeValues (a: int64) (b: int64) =
    if a % bitModulo = b % bitModulo then 1 else 0

let rec run (a: int64) (b: int64) step max judge =
    if step = max then judge
    else 
        let ma = a * genA % modulo
        let mb = b * genB % modulo
        run ma mb (step + 1) max (judge + (judgeValues ma mb))

run startA startB 0 40000000 0
