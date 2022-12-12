module AdventOfCode.Day6

let private today = Day.Day6
let private phase = Data.Prod

let solve (windowSize: int) (line: string) =
    let allDifferent (items: char array) = Array.distinct items = items    
    let index =
        line.ToCharArray ()
        |> Array.windowed windowSize
        |> Array.findIndex allDifferent
    index + windowSize

let private part1 () =
    let data = Data.load phase today 1
    Array.map (solve 4) data
    
let private part2 () =
    let data = Data.load phase today 1
    Array.map (solve 14) data

let run () =
    part1 () |> printfn "Start of Packet: \"%A\""
    part2 () |> printfn "Start of Message: \"%A\""