module AdventOfCode.Day9

let private today = Day.Day9
let private phase = Data.Prod

[<RequireQualifiedAccess>]
type private RopeMovement =
    | Right
    | Down
    | Left
    | Up

let private parseLine (input: string) : (RopeMovement * int) option =
    match input with
    | Util.Regex "([RDLU])\s(\d*)" ["R"; Util.Int num] -> Some (RopeMovement.Right, num)
    | Util.Regex "([RDLU])\s(\d*)" ["D"; Util.Int num] -> Some (RopeMovement.Down, num)
    | Util.Regex "([RDLU])\s(\d*)" ["L"; Util.Int num] -> Some (RopeMovement.Left, num)
    | Util.Regex "([RDLU])\s(\d*)" ["U"; Util.Int num] -> Some (RopeMovement.Up, num)
    | other ->
        printfn $"Unknown input: %s{other}"
        None

type MovementHistory =
    {
        HeadPosition: int * int
        TailPosition: int * int
        History: ((int * int) * (int * int)) array
        Debug: ((int * int) * (int * int) * (int * int)) array
    }

[<RequireQualifiedAccess>]
module MovementHistory =
    let empty : MovementHistory =
        {
            HeadPosition = (0, 0)
            TailPosition = (0, 0)
            History = Array.empty
            Debug = Array.empty
        }
    
    let uniqueTailPositions (history: MovementHistory) =
        Array.distinctBy snd history.History

let private moveHead (history: MovementHistory) (movement: RopeMovement) : MovementHistory =
    let newHead =
        let hx, hy = history.HeadPosition
        match movement with
        | RopeMovement.Right -> hx+1, hy
        | RopeMovement.Down -> hx, hy-1
        | RopeMovement.Left -> hx-1, hy
        | RopeMovement.Up -> hx, hy+1
    
    let nhx, nhy = newHead
    let tx, ty = history.TailPosition
    let dx = nhx - tx
    let dy = nhy - ty
    
    let newTail =
        // In the same position or close enough
        if abs dx <= 1 && abs dy <= 1 then
            tx, ty
        elif abs dx > 1 && dy = 0 then
            let dirX = dx / abs dx
            tx+dirX, ty
        elif abs dy > 1 && dx = 0 then
            let dirY = dy / abs dy
            tx, ty+dirY
        else
            let dirX = dx / abs dx
            let dirY = dy / abs dy
            tx+dirX, ty + dirY
    
    let newHistory =
        Array.append history.History [| newHead, newTail |]
    
    let newDebugEntry = [| newHead, newTail, (dx, dy) |]
    
    {
        HeadPosition = newHead
        TailPosition = newTail
        History = newHistory
        Debug = Array.append history.Debug newDebugEntry
    }

let private expandMovement (movement: RopeMovement, amount: int) : RopeMovement array =
    Array.init amount (fun _ -> movement)

let private part1 () =
    let data = Data.load phase today 1
    data
    |> Array.choose parseLine
    |> Array.collect expandMovement
    |> Array.fold moveHead MovementHistory.empty
    |> MovementHistory.uniqueTailPositions
    |> Array.length

let private part2 () =
    -1

let run () =
    part1 () |> printfn "Unique Tail positions: \"%A\""
    part2 () |> printfn "TODO: \"%A\""