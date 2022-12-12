module AdventOfCode.Day3

let x : int option = None

let private today = Day.Day3
let private phase = Data.Prod

let private parseLineSplit (input: string) =
    let len = input.Length
    let first = input.Substring (0, len >>> 1)
    let second = input.Substring (len >>> 1)
    first.ToCharArray (), second.ToCharArray ()
    
let private parseLine (input: string) = input.ToCharArray ()

let private sharedChars (first: Set<char>, second: Set<char>) =
    Set.intersect first second
    
let private sharedCharsArr (items: Set<char> array) =
    Array.fold Set.intersect items[0] items

let private priority (input: char) =
    let numRep = int input
    if System.Char.IsLetter input then
        if System.Char.IsLower input then
            Some (numRep - 96)
        else
            Some (numRep - 38)
    else
        None

let private part1 () =
    let data = Data.load phase today 1
    data
    |> Array.map parseLineSplit
    |> Array.map (fun (f, s) -> set f, set s)
    |> Array.map sharedChars
    |> Array.collect Set.toArray
    |> Array.choose priority
    |> Array.sum
    
let private part2 () =
    let data = Data.load phase today 1
    data
    |> Array.map parseLine
    |> Array.chunkBySize 3
    |> Array.map (Array.map set)
    |> Array.map sharedCharsArr
    |> Array.collect Set.toArray
    |> Array.choose priority
    |> Array.sum

let run () =
    part1 () |> printfn "Shared in compartments: \"%A\""
    part2 () |> printfn "Badge type: \"%A\""