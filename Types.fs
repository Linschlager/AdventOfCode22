namespace AdventOfCode

[<RequireQualifiedAccess>]
type Day =
    | Day1
    | Day2
    | Day3
    | Day4
    | Day5
    | Day6
    | Day7
    | Day8
    | Day9
    | DayX

[<RequireQualifiedAccess>]
module Util =
    let (|Int|_|) (inputString: string) =
        match System.Int32.TryParse inputString with
        | true, output -> Some output
        | false, _ -> None
    
    /// Regex Active pattern from http://www.fssnip.net/29/title/Regular-expression-active-pattern
    /// usage:
    /// match x with
    /// | Regex @"pattern" [list; of; capture;groups] -> ()
    /// | _ -> printf "No matches"
    let (|Regex|_|) pattern input =
        let m = System.Text.RegularExpressions.Regex.Match (input, pattern)
        if m.Success then        
            List.tail [
                for g in m.Groups do
                    g.Value
            ]
            |> Some
        else
            None