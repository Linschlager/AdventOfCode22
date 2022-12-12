module AdventOfCode.Day1

let private today = Day.Day1
let private phase = Data.Prod

let private totalCaloriesPerElf () =
    let data = Data.load phase today 1
    let scanner (state: int array array) (entry: string) =
        match entry with
        | "" -> Array.append [|Array.empty|] state
        | Util.Int next ->
            let head = Array.head state
            let newHead = Array.append head [| next |]
            Array.updateAt 0 newHead state
        | other ->
            failwith $"Unexpected entry: %A{other}"
            
    let elves =
        data
        |> Array.fold scanner [| Array.empty |]
        |> Array.rev
    
    let totals =
        elves
        |> Array.map Array.sum
    totals
    
let private part1 () =
    totalCaloriesPerElf ()
    |> Array.max
    
let private part2 () =
    totalCaloriesPerElf ()
    |> Array.sortDescending
    |> Array.take 3
    |> Array.sum

let run () =
    part1 () |> printfn "Highest number of calories: \"%A\""
    part2 () |> printfn "Top 3 Total: \"%A\""