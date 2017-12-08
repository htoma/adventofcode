module Strings
open System
    let inline explode (s: string) = [for c in s -> c]

    let inline charToInt c = int c - int '0'

    let inline toNumber s = Int32.Parse s