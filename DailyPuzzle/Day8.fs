module AdventOfCode.Day8

let private today = Day.Day8
let private phase = Data.Prod

let private parseLine (input: string) =
    input.ToCharArray ()
    |> Array.map int

let private isVisible (x: int) (y: int) (item: int) (grid: int [,]) =    
    // Edge Trees are always visible
    if x = 0 || y = 0 then true
    elif Array2D.length1 grid - 1 = x || Array2D.length2 grid - 1 = y then true
    else
        // are there ONLY lower trees to the left, top, right, bottom
        let isVisible (los: int array) = Array.max los < item
        let above = grid[x, .. y - 1]
        let below = grid[x, y + 1 ..]
        let left = grid[.. x - 1, y]
        let right = grid[x + 1 .., y]
        
        isVisible above ||
        isVisible below ||
        isVisible left ||
        isVisible right

let private scenicDistance (grid: int [,]) (x: int) (y: int) (item: int) =
    if x = 0 || y = 0 then 0
    elif Array2D.length1 grid - 1 = x || Array2D.length2 grid - 1 = y then 0
    else
        let isHighest (otherTree: int) = item > otherTree
        let above = Array.rev grid[x, .. y - 1]
        let below = grid[x, y + 1 ..]
        let left = Array.rev grid[.. x - 1, y]
        let right = grid[x + 1 .., y]
        
        let distanceTo (otherTrees: int array) =
            let total =
                otherTrees
                |> Array.takeWhile isHighest
                |> Array.length
            if otherTrees.Length = total then total
            else total+1
        distanceTo above * distanceTo below * distanceTo left * distanceTo right

let private part1 () =
    let data = Data.load phase today 1
    let grid =
        data
        |> Array.map parseLine
        |> array2D
    
    let visibleTrees =
        Array2D.mapi (fun x y e ->
            if isVisible x y e grid then 1
            else 0
            ) grid
    
    visibleTrees
    |> Seq.cast<int>
    |> Seq.sum

let private part2 () =
    let data = Data.load phase today 1
    let grid =
        data
        |> Array.map parseLine
        |> array2D
    
    let visibleTrees =
        Array2D.mapi (scenicDistance grid) grid
    
    visibleTrees
    |> Seq.cast<int>
    |> Seq.max

let run () =
    part1 () |> printfn "Visible Trees from outside: \"%A\""
    part2 () |> printfn "Max Scenic Distance: \"%A\""