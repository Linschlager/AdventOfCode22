module AdventOfCode.Day4

let private today = Day.Day4
let private phase = Data.Prod

type Range = { Start: int; End: int }

[<RequireQualifiedAccess>]
module Range =
    let init start end' = { Start = start; End = end' }
    let fullyContains (r1: Range) (r2: Range) =
        r1.Start <= r2.Start && r1.End >= r2.End
    
    let overlaps (r1: Range) (r2: Range) =
        
        if r1.Start < r2.Start && r1.End < r2.Start then false
        elif r2.Start < r1.Start && r2.End < r1.Start then false
        else
            true
    
    let tryParse (inputStr: string) =
        match inputStr.Split "-" with
        | [| Util.Int start; Util.Int end' |] -> init start end' |> Some
        | _ -> None
        
    let (|Parse|_|) = tryParse

let private parseLine (input: string) =
    let sections = input.Split ","
    match sections with
    | [| Range.Parse first; Range.Parse second |] ->
        first, second
    | other ->
        failwith $"Invalid line: %A{other}"

let private part1 () =
    let data = Data.load phase today 1
    data
    |> Array.map parseLine
    |> Array.sumBy (fun (r1, r2) ->
       if Range.fullyContains r1 r2 then 1
       elif Range.fullyContains r2 r1 then 1
       else 0
       )
    
let private part2 () =
    let data = Data.load phase today 1
    data
    |> Array.map parseLine
    |> Array.sumBy (fun (r1, r2) ->
       if Range.overlaps r1 r2 then 1
       else 0
    )

let testCases : (unit -> bool) array =
    [|
        Range.init 1 2, Range.init 2 4, true
        Range.init 2 3, Range.init 1 2, true
    |]
    |> Array.map (fun (r1, r2, result) -> fun () -> Range.overlaps r1 r2 = result)

let run () =
    do
        printfn "Running tests..."
        testCases
        |> Array.iteri (fun i test ->
            if test () then ()
            else printfn $"%i{i} failed!")     
    
    part1 () |> printfn "Fully contained sections: \"%A\""
    part2 () |> printfn "Overlapping sections: \"%A\""