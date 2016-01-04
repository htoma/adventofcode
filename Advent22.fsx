open System

type Spell = 
    {
        Name: string
        Cost: int
        Damage: int
        Armor: int
        Healing: int
        Mana: int
        IsImmediate: bool
        TurnsLeft: int
    }

let buildSpell name cost weapon armor healing money isImmediate turnsLeft = 
 {Name = name; Cost = cost; Damage = weapon; Armor = armor; Healing = healing; Mana = money; IsImmediate = isImmediate; TurnsLeft = turnsLeft }

let shop =
          [ buildSpell "Magic Missile" 53 4 0 0 0 true 0
            buildSpell "Drain" 73 2 0 2 0 true 0
            buildSpell "Shield" 113 0 7 0 0 false 6
            buildSpell "Poison" 173 3 0 0 0 false 6
            buildSpell "Recharge" 229 0 0 0 101 false 5]

type PlayerType = 
    | You
    | Boss

type Player = 
    { 
      Type: PlayerType      
      HitPoints: int
      Weapon: int
      Mana: int
      Spending: int
      Spells: Spell list
    }

let you =  
    {
        Type = You
        HitPoints = 50
        Weapon = 0
        Mana = 500
        Spending = 0
        Spells = List.empty
    }

let boss = 
    {
        Type = Boss
        HitPoints = 51
        Weapon = 9
        Mana = 0
        Spending = 0
        Spells = List.empty
    }

type Turn = {Turn: PlayerType}

let printStats (you: Player,boss: Player) =
     printfn "You have life: %i attack %i and mana %i" you.HitPoints you.Weapon you.Mana
     printfn "Boss has life: %i, attack %i" boss.HitPoints boss.Weapon

let printSpell (s: Spell) =
    printfn "Spell %s that costs %i and is immediate %A (%i), hit %i, armor %i, heal %i, mana %i" s.Name s.Cost s.IsImmediate s.TurnsLeft s.Damage s.Armor s.Healing s.Mana

let castImmediateSpell (spell: Spell) (you: Player,boss: Player) =
    //printfn "Cast spell"
    //printSpell spell
    if not(spell.IsImmediate) then you,boss
    else        
        let y = {you with HitPoints=you.HitPoints+spell.Healing}
        let b = {boss with HitPoints=boss.HitPoints-spell.Damage}
        //printStats (y,b)
        y,b

let executeSpell (spell: Spell) (you: Player,boss: Player) =
    //printfn "Executing spell"
    //printSpell spell
    let b = {boss with HitPoints=boss.HitPoints-spell.Damage}
    let spells = match spell.TurnsLeft with
                 | x when x<=1 -> you.Spells |> List.filter (fun v -> v.Name<>spell.Name)
                 | _ -> you.Spells |> List.map (fun v -> match v.Name with 
                                                         | x when x=spell.Name -> {v with TurnsLeft=v.TurnsLeft-1}
                                                         | _ -> v )
    {you with Mana=you.Mana+spell.Mana;HitPoints=you.HitPoints+spell.Healing;Spells=spells},b

let executeSpells (you: Player,boss: Player) =
    let rec loop (spells: Spell list) (y: Player,b: Player) =
        match spells with
        | [] -> (y,b)
        | s::tail -> 
            let (yH,bH) = executeSpell s (y,b)
            loop tail (yH, bH)
    loop you.Spells (you,boss)

let attack (turn: Turn) (you: Player,boss: Player) =    
    if turn.Turn=PlayerType.You then
        //printfn "You attacking boss"
        let b = {boss with HitPoints = boss.HitPoints-you.Weapon}
        you,b
    else
        //printfn "Boss attacking you"
        let y = {you with HitPoints = you.HitPoints-Math.Max(1,boss.Weapon-(you.Spells |> List.sumBy (fun v -> v.Armor)))}
        y,boss         
 
let mutable min = 1500
    
let run you boss = 
    let rec loop (youP: Player) (boss: Player) (turn: Turn) (spellList: string list) = 
        //printfn "Who's playing: %A" turn
        //printStats (you,boss)
        let you = {youP with HitPoints=youP.HitPoints-1}               
        if you.HitPoints<=0
        then 
            //printfn "You're dead"
            [(boss.Type,you.Spending,spellList)]
        elif boss.HitPoints<=0 then 
            //printfn "Boss is dead"
            if you.Spending<min then min<-you.Spending
            [(you.Type,you.Spending,spellList)]
        else
            let (youS,bossS) = executeSpells (you,boss)
            //printfn "After executing all existing spells"
            //printStats (youS,bossS)
            if bossS.HitPoints<=0 then 
                //printfn "Boss is dead after executing spells"                
                if you.Spending<min then min<-you.Spending  
                [(youS.Type,youS.Spending,spellList)]
            else
              let youA,bossA = attack turn (youS,bossS)
              //printStats (youA,bossA)
              if youA.HitPoints<=0 
              then 
                //printfn "You're dead after the attack"
                [(bossA.Type,youA.Spending,spellList)]
              elif bossA.HitPoints<=0 then 
                //printfn "Boss is dead after the attack"                
                if you.Spending<min then min<-you.Spending
                [(youA.Type,youA.Spending,spellList)]
              else 
                let newTurn = if turn.Turn=PlayerType.You 
                              then {Turn=PlayerType.Boss}
                              else {Turn=PlayerType.You}  
                match turn.Turn with 
                | PlayerType.Boss -> loop youA bossA newTurn spellList
                | PlayerType.You ->
                                let spells = shop
                                             |> List.filter (fun v -> youA.Mana>=v.Cost && not(List.exists (fun el -> el.Name=v.Name) youA.Spells))
                                if spells.Length=0 then                                      
                                     //printfn "You're dead cause you're out of money"
                                     [(bossA.Type,youA.Spending,spellList)]
                                else                                                                               
                                     spells
                                     //|> List.take (Math.Min(4, spells.Length)) //delete this
                                     |> List.map (fun v ->
                                            if youA.Spending + v.Cost > min 
                                            then [(bossA.Type,youA.Spending + v.Cost,spellList)]                                                                       
                                            else
                                                if v.IsImmediate then 
                                                    let (y,b) = castImmediateSpell v ({youA with Mana=youA.Mana-v.Cost; Spending=youA.Spending+v.Cost},bossA)
                                                    loop y b newTurn (v.Name::spellList)
                                                else
                                                    loop {youA with Mana=youA.Mana-v.Cost; Spending=youA.Spending+v.Cost;Spells=v::youA.Spells} bossA newTurn (v.Name::spellList))
                                     |> List.concat
        
    loop you boss {Turn=You} []

run you boss
|> List.filter (fun (t,c,s) -> t=PlayerType.You)
|> List.minBy (fun (_,c,_) -> c)
