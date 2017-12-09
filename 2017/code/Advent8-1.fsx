open System.Web.Services.Protocols
#load "../Strings.fsx"

open System
open Strings
open System.Text.RegularExpressions

type Register = 
    { Name: string
      Value: int }

type Operation =
    | Inc
    | Dec

type ConditionOperation =
    | Less
    | LessEqual
    | Equal
    | BiggerEqual
    | Bigger
    | NotEqual

type Condition =
    { Name: string
      Op: ConditionOperation
      Value: int }
    
type Expression =
    { Reg: string
      Op: Operation
      Value: int
      Cond: Condition }

let parseOperation text =
    match text with
    | "inc" -> Operation.Inc
    | "dec" -> Operation.Dec

let parseConditionOperation text =
    match text with
    | "<" -> ConditionOperation.Less
    | "<=" -> ConditionOperation.LessEqual
    | "==" -> ConditionOperation.Equal
    | ">=" -> ConditionOperation.BiggerEqual
    | ">" -> ConditionOperation.Bigger
    | "!=" -> ConditionOperation.NotEqual

let parseLine (line: string) =
    let m = Regex.Match(line, @"^(\w+) (\w+) ([-\d]+) if (\w+) ([<>=!]+) ([-\d]+)")
    { Reg = m.Groups.[1].Value
      Op = m.Groups.[2].Value |> parseOperation
      Value = m.Groups.[3].Value |> Int32.Parse
      Cond = { Name = m.Groups.[4].Value
               Op = m.Groups.[5].Value |> parseConditionOperation
               Value = m.Groups.[6].Value |> Int32.Parse } }

let matchesCondition (reg: Register) (cond: Condition) = 
    if reg.Name <> cond.Name then false
    else 
        match cond.Op with
        | ConditionOperation.Less -> reg.Value < cond.Value
        | ConditionOperation.LessEqual  -> reg.Value <= cond.Value
        | ConditionOperation.Equal  -> reg.Value = cond.Value
        | ConditionOperation.BiggerEqual  -> reg.Value >= cond.Value
        | ConditionOperation.Bigger  -> reg.Value > cond.Value
        | ConditionOperation.NotEqual  -> reg.Value <> cond.Value

let update (op: Operation) (value: int) (reg: Register) =
    match op with
    | Operation.Inc ->  {reg with Value = reg.Value + value}
    | Operation.Dec ->  {reg with Value = reg.Value - value}

let addRegIfNeeded reg (registers: Register list) =
    if registers |> List.exists (fun r -> r.Name = reg) |> not 
    then { Name = reg; Value = 0}::registers 
    else registers

let rec workItOut (expressions: Expression list) (registers: Register list) =
    match expressions, registers with
    | [], [] -> 0
    | [], l -> let reg = l |> List.maxBy (fun r -> r.Value)
               reg.Value
    | exp::tail, _ ->
        let newRegisters = (registers |> addRegIfNeeded exp.Reg) |> addRegIfNeeded exp.Cond.Name
        if newRegisters |> List.exists (fun r -> matchesCondition r exp.Cond) |> not
        then workItOut tail newRegisters
        else
            workItOut tail (newRegisters |> List.map (fun r -> if r.Name = exp.Reg then r |> update exp.Op exp.Value else r))

let expressions = IO.File.ReadAllLines(@"2017\data\Advent8-1.txt")
                  |> Array.map parseLine
                  |> List.ofArray

workItOut expressions []