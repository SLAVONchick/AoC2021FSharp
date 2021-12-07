

open System
open System.IO

type Crabs = int seq

let median (crabs: Crabs) =
    let len = crabs |> Seq.length
    crabs
    |> Seq.sort
    |> Seq.item (len / 2 + len % 2)
    
let leastFuelCost (crabs: Crabs) =
    let pos = median crabs
    crabs
    |> Seq.map (fun crab -> let cost = crab - pos in if cost < 0 then -cost else cost)
    |> Seq.sum
    
let test (crabs: Crabs) (estLeastCost: int) =
    let realLeastCost = 
        crabs
        |> Seq.map (fun pos ->
            crabs
            |> Seq.map (fun crab -> let cost = crab - pos in if cost < 0 then -cost else cost)
            |> Seq.sum)
        |> Seq.sort
        |> Seq.head
    realLeastCost = estLeastCost

// For more information see https://aka.ms/fsharp-console-apps
let readInput () : Crabs =
    (File.ReadAllText "input")
        .Split(',')
    |> Seq.map Int32.Parse
    

let input = readInput ()
let res = leastFuelCost input

Console.WriteLine $"Least fuel cost = {res}"
let yes = "Yes!"
let no = "No!"

if not << String.IsNullOrEmpty <| Environment.GetEnvironmentVariable "DAY7_TEST"
then 
    Console.WriteLine $"Is it really the least cost? {if test input res then yes else no}" 