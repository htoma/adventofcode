module Strings
    let inline explode (s: string) = [for c in s -> c]

    let inline charToInt c = int c - int '0'