#load "../Strings.fsx"

open System
open Strings
open System.Text.RegularExpressions

type Coord =
    { x: int
      y: int
      z: int }

type Particle = 
    { index: int
      pos: Coord
      vel: Coord
      acc: Coord
      isStable: bool }

let toCoord (s: string) =
    let v = s.Split ','
    {x = v.[0] |> toNumber; y = v.[1] |> toNumber; z = v.[2] |> toNumber}

let isStable (vel: Coord) (acc: Coord) =
    vel.x * acc.x >= 0 && vel.y * acc.y >= 0 && vel.z * acc.z >= 0

let computeDist (p: Coord) =
    Math.Abs(p.x) + Math.Abs(p.y) + Math.Abs(p.z)

let updateParticle (p: Particle) =
    let vel = {x = p.vel.x + p.acc.x; y = p.vel.y + p.acc.y; z = p.vel.z + p.acc.z}
    let pos = {x = p.pos.x + vel.x; y = p.pos.y + vel.y; z = p.pos.z + vel.z}
    {p with pos = pos; vel = vel; isStable = isStable vel p.acc}

let rec simulate (particles: Particle list) =
    match particles |> List.tryFind (fun p -> p.isStable |> not) with
    | Some _ -> particles |> List.map updateParticle |> simulate
    | None -> // everything stable
              particles |> List.minBy (fun p -> computeDist p.acc)

let rec collidingTrip (particles: Particle list) epochs current =
    match particles, current with
    | [], _ -> 0
    | _, v when v = epochs -> particles.Length
    | _ -> 
            let updated = particles 
                        |> List.map updateParticle
            let check = updated
                        |> List.groupBy (fun p -> p.pos)
                        |> List.filter (fun (_, g) -> g.Length > 1)
                        |> List.map (fun (_, g) -> g |> List.map (fun p -> p.index))
                        |> List.concat
                        |> List.distinct
                        |> Set.ofList
            collidingTrip (updated |> List.filter (fun p -> check.Contains p.index |> not)) epochs (current + 1)
                
let parseLine index line =
    let m = Regex.Match(line, @"^p=<([,-\\d]+)>, v=<([,-\\d]+)>, a=<([,-\\d]+)>")
    { index = index; pos = m.Groups.[1].Value |> toCoord; vel = m.Groups.[2].Value |> toCoord; acc = m.Groups.[3].Value |> toCoord; isStable = false}

let particles = IO.File.ReadAllLines(@"2017\data\Advent20.txt")
             |> Array.mapi parseLine
             |> List.ofArray

//part 1: simulate particles
//part2: collidingTrip particles 5000 0


