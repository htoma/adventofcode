open System
open System.Text.RegularExpressions

type CommandName = 
    | Inc
    | Hlf
    | Tpl
    | Jmp
    | Jio
    | Jie

type Command =
    { Name: CommandName
      Register: string
      Jump: int }

let parseLine (line: string) = 
    let split = line.Split ','
    let res = Regex.Match(split.[0], @"(\w+) ((\w+)|([-+0-9]+))")
    match res.Groups.[1].Value with
    | "inc" -> 
                {Name = CommandName.Inc; Register = res.Groups.[2].Value; Jump=0}
    | "hlf" -> 
                {Name = CommandName.Hlf; Register = res.Groups.[2].Value; Jump=0}
    | "tpl" -> 
                {Name = CommandName.Tpl; Register = res.Groups.[2].Value; Jump=0}
    | "jmp" ->  
                {Name = CommandName.Jmp; Register = ""; Jump=Int32.Parse res.Groups.[2].Value}
    | "jio" -> 
                {Name = CommandName.Jio; Register = res.Groups.[2].Value; Jump=Int32.Parse (split.[1].TrimStart())}
    | "jie" -> 
                {Name = CommandName.Jie; Register = res.Groups.[2].Value; Jump=Int32.Parse (split.[1].TrimStart())}    
    | _ -> 
        failwith "Unrecognized command"
    
let content = IO.File.ReadAllLines(@"c:\temp\23.txt")

let commands = content |> Array.map (fun l -> l |>parseLine)

let rec goOver (commands: Command[]) (pos: int) (regs: (string*int) list) =
    match pos with
    | x when x<0 || x >= commands.Length -> regs
    | i -> 
         let command = commands.[pos]
         match command.Name with
         | CommandName.Jmp -> 
                printfn "Jumping %i" command.Jump
                goOver commands (pos+command.Jump) regs
         | CommandName.Hlf -> 
                            match List.tryFind (fun (n,_) -> n=command.Register) regs with
                            | None -> goOver commands (pos+1) ((command.Register,0)::regs)
                            | Some (n,v) -> 
                                printfn "Halfing %i" v    
                                goOver commands (pos+1) ((n,v/2)::(List.except [(n,v)] regs))
         | CommandName.Inc -> 
                            match List.tryFind (fun (n,_) -> n=command.Register) regs with
                            | None -> goOver commands (pos+1) ((command.Register,1)::regs)
                            | Some (n,v) -> 
                                printfn "Incrementing %i" v
                                goOver commands (pos+1) ((n,v+1)::(List.except [(n,v)] regs))
         | CommandName.Tpl -> 
                            match List.tryFind (fun (n,_) -> n=command.Register) regs with
                            | None -> goOver commands (pos+1) ((command.Register,0)::regs)
                            | Some (n,v) -> 
                                printfn "Tripling %i" v
                                goOver commands (pos+1) ((n,v*3)::(List.except [(n,v)] regs))
         | CommandName.Jie -> 
                            match List.tryFind (fun (n,_) -> n=command.Register) regs with
                            | Some (n,v) when v%2=0 -> 
                                printfn "Jieing %i" command.Jump
                                goOver commands (pos+command.Jump) regs
                            | _ -> goOver commands (pos+1) regs
         | CommandName.Jio -> 
                            match List.tryFind (fun (n,_) -> n=command.Register) regs with
                            | Some (n,v) when v=1 -> 
                                printfn "Jioing %i" command.Jump
                                goOver commands (pos+command.Jump) regs
                            | _ -> goOver commands (pos+1) regs

goOver commands 0 [("a",1)]
