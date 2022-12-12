module AdventOfCode.Program

let private runPuzzle =
    function
    | Day.Day1 -> Day1.run ()
    | Day.Day2 -> Day2.run ()
    | Day.Day3 -> Day3.run ()
    | Day.Day4 -> Day4.run ()
    | Day.Day5 -> Day5.run ()
    | Day.Day6 -> Day6.run ()
    | Day.Day7 -> Day7.run ()
    | Day.Day8 -> Day8.run ()
    | Day.Day9 -> Day9.run ()
    | Day.Day10 -> Day10.run ()
    | Day.DayX -> DayX.run ()
    
let inline all<'a> = 
    Reflection.FSharpType.GetUnionCases typeof<'a>
    |> Array.map (fun uc -> (Reflection.FSharpValue.MakeUnion (uc, Array.empty) :?> 'a))
    
let main () : unit =
    let allDays =
        Reflection.FSharpType.GetUnionCases typeof<Day>
        |> Array.map (fun uc -> (Reflection.FSharpValue.MakeUnion (uc, Array.empty) :?> Day))
    
    let dayToRun =
        allDays
        |> Array.item (allDays.Length - 2)
    
    runPuzzle dayToRun