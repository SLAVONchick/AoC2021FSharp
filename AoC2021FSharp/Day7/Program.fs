

open System
open System.IO

type Crabs = int seq

let median (crabs: Crabs) =
    let len = crabs |> Seq.length
    crabs
    |> Seq.sort
    |> Seq.item (len / 2 + len % 2)
    
let avg (crabs: Crabs) =
    let len = crabs |> Seq.length
    let sum = crabs |> Seq.sum
    sum / len
    
let progression (n: int) =
    seq { for i in 1..n -> i }
    |> Seq.sum

let findCost pos crab =
    let cost = crab - pos
    if cost < 0 then progression -cost else progression cost

let leastFuelCost (crabs: Crabs) =
    let pos = avg crabs
    crabs
    |> Seq.map (findCost pos) 
    |> Seq.sum
    
let test (crabs: Crabs) (estLeastCost: int) =
    let realLeastCost = 
        crabs
        |> Seq.map (fun pos ->
            crabs
            |> Seq.map (findCost pos)
            |> Seq.sum)
        |> Seq.sort
        |> Seq.head
    realLeastCost = estLeastCost

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