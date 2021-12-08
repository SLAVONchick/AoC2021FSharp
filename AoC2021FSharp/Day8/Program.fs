open System
open System.Diagnostics
open System.IO

let simpleNumbers = set [2; 3; 4; 7]

let stringEquals (one: char seq) (other: string) =
    one
    |> Seq.exists (not << other.Contains)
    |> not

/// <summary>
/// Represents a display with seven segments: <br/>
///
///      _____<br/>
///     |  1  |<br/>
///    2|     |3<br/>
///     |_____|<br/>
///     |  4  |<br/>
///    5|     |6<br/>
///     |_____|<br/>
///        7
///
///</summary>
type Display private() =    
    member val private first = ' ' with get, set
    member val private second = ' ' with get, set
    member val private third = ' ' with get, set
    member val private fourth = ' ' with get, set
    member val private fifth = ' ' with get, set
    member val private sixth = ' ' with get, set
    member val private seventh = ' ' with get, set
    
    
    
    member private self.zero with get() = set [self.first; self.second; self.third; self.fifth; self.sixth; self.seventh]
    member private self.one with get() = set [self.third; self.sixth]
    member private self.two with get() = set [self.first; self.third; self.fourth; self.fifth; self.seventh]
    member private self.three with get() = set [self.first; self.third; self.fourth; self.sixth; self.seventh]
    member private self.four with get() = set [self.second; self.third; self.fourth; self.sixth]
    member private self.five with get() = set [self.first; self.second; self.fourth; self.sixth; self.seventh]
    member private self.six with get() = set [self.first; self.second; self.fourth; self.fifth; self.sixth; self.seventh]
    member private self.seven with get() = set [self.first; self.third; self.sixth]
    member private self.eight with get() = set [self.first; self.second; self.third; self.fourth; self.fifth; self.sixth; self.seventh]
    member private self.nine with get() = set [self.first; self.second; self.third; self.fourth; self.sixth; self.seventh]
    
    static member Create(signals: string[]) =
        let display = Display()
        let one = signals |> Seq.find (fun it -> it.Length = 2 )
        display.third <- one.[0]
        display.sixth <- one.[1]

        let four = signals |> Seq.find (fun it -> it.Length = 4 )
        let fourWithoutAlreadyKnownDigits =
            four.Replace(display.third.ToString(), "")
                .Replace(display.sixth.ToString(), "")
        display.second <- fourWithoutAlreadyKnownDigits.[0]
        display.fourth <- fourWithoutAlreadyKnownDigits.[1]

        let seven =
            let seven' = signals |> Seq.find (fun it -> it.Length = 3)
            seven'.Replace(display.third.ToString(), "")
                .Replace(display.sixth.ToString(), "")
        display.first <- seven.[0]

        let knownSegments = set [
            display.first;
            display.second;
            display.third;
            display.fourth;
            display.sixth
        ]

        let eight =
            signals
            |> Seq.find (fun it -> it.Length = 7)
            |> Seq.filter (not << knownSegments.Contains)
            |> Seq.toArray
        display.fifth <- eight.[0]
        display.seventh <- eight.[1]

        let mutable success = true

        let mutable iterCount = 1
        while success && iterCount <= 7 do
            let mutable i = 0
            while success && i < signals.Length do
                if display.IsNumber(signals.[i]).IsNone
                then
                    success <- false
                i <- i + 1
            if not success then
                if iterCount <= 7 then
                    // here we need to swap some of the recently set characters with each other
                    // till every signal in signals makes sense (display.IsNumber)
                    // and there three of such pairs of characters, so we represent them
                    // as a binary 7 (111), so that at 001 we swap the first pair,
                    // at 010 we swap first pair back and second pair forward, at 011 we swap first pair
                    // forward again and so on, till we have tried every combination of those 
                    match iterCount with
                    // 001
                    | 1 -> 
                        display.fourth <- fourWithoutAlreadyKnownDigits.[0]
                        display.second <- fourWithoutAlreadyKnownDigits.[1]                        
                    // 011
                    | 2 -> 
                        display.sixth <- one.[0]
                        display.third <- one.[1]                    
                    // 111
                    | 3 -> 
                        display.seventh <- eight.[0]
                        display.fifth <- eight.[1]
                    // 101
                    | 4 ->
                        display.third <- one.[0]
                        display.sixth <- one.[1]
                    // 100
                    | 5 -> 
                        display.second <- fourWithoutAlreadyKnownDigits.[0]
                        display.fourth <- fourWithoutAlreadyKnownDigits.[1]
                    // 110
                    | 6 -> 
                        display.sixth <- one.[0]
                        display.third <- one.[1]
                    // 010
                    | 7 ->
                        display.fifth <- eight.[0]
                        display.seventh <- eight.[1]
                    success <- true

            iterCount <- iterCount + 1

        if not success then
            let notParsed = signals |> Seq.filter (fun it -> display.IsNumber(it).IsNone )
            let sep = " "
            raise <| Exception(
                $"NO SUCCESS IN DETERMINING SIGNALS {
                  String.Join(sep, signals)} (NOT PARSED VALUES: {
                  String.Join(sep, notParsed)}) \n%O{display}\n")
        
        display
        
    member self.IsNumber (input: string) : int option =
        match input.Length with
        | 2 -> if stringEquals self.one input then Some 1 else None
        | 3 -> if stringEquals self.seven input then Some 7 else None
        | 4 -> if stringEquals self.four input then Some 4 else None
        | 7 -> if stringEquals self.eight input then Some 8 else None
        | 5 ->
            if stringEquals self.two input then Some 2
            elif stringEquals self.three input then Some 3
            elif stringEquals self.five input then Some 5
            else None
        | 6 ->
            if stringEquals self.zero input then Some 0
            else if stringEquals self.six input then Some 6
            else if stringEquals self.nine input then Some 9
            else None
        | _ -> None
            
    override self.ToString() =
        "     _____\n" +
        $"    |  {self.first}  |\n" +
        $"   {self.second}|     |{self.third}\n" +
        "    |_____|\n" +
        $"    |  {self.fourth}  |\n" +
        $"   {self.fifth}|     |{self.sixth}\n" +
        "    |_____|\n" +
        $"       {self.seventh}   \n"
            
let countSimpleDigits (output: string seq) =
    output
    |> Seq.filter (fun it -> simpleNumbers.Contains it.Length)
    |> Seq.length
    
let part1 (input: string seq) =
    input
    |> Seq.collect (fun it -> it.Split('|').[1].Split(' '))
    |> countSimpleDigits
    
let part2 (inputs: string seq) =
    inputs
    |> Seq.fold (fun i s ->
        let split = s.Split('|')
        let signals =
            split.[0].Split ' '
            |> Seq.map (fun it -> it.Trim())
            |> Seq.filter (not << String.IsNullOrWhiteSpace)
        let display = Display.Create (Seq.toArray signals)
        let outputs =
            split.[1].Split ' '
            |> Seq.map (fun it -> it.Trim())
            |> Seq.filter (not << String.IsNullOrWhiteSpace)
        let num =
            outputs
            |> Seq.fold (fun n o ->
                let digit =
                    display.IsNumber o
                    |> Option.get
                n * 10 + digit) 0
        i + num
        ) 0
    
    
let input = File.ReadAllLines "input.txt"

let part1Result = part1 input
Console.WriteLine part1Result
Debug.Assert((part1Result = 375))

let part2Result = part2 input
Console.WriteLine part2Result
Debug.Assert((part2Result = 1019355))