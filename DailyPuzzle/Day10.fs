module AdventOfCode.Day10

let private today = Day.Day10
let private phase = Data.Prod

type private Instr =
    | NoOp
    | AddX of int

let private parseLine (input: string) : Instr option =
    match input with
    | "noop" -> Some Instr.NoOp
    | Util.Regex "addx (-?\d+)" [ Util.Int num ] -> Some (Instr.AddX num)
    | _ -> None

let private emptyLine : char array =
    Array.init 40 (fun _ -> '.')

let private compute (state: int, intermediate: int array) (next: Instr) =
    match next with
    | Instr.NoOp -> state, Array.append intermediate [| state |]
    | Instr.AddX num ->
        let newState = state + num
        let newHist = Array.append intermediate [| state; newState |]
        newState, newHist

let private sample (offset: int) (every: int) (array: int array) : (int * int) array =
    let numSamples = (Array.length array - offset) / every + 1
    let indices = Array.init numSamples (fun x -> x * every + offset - 1)
    Array.map (Array.get array) indices
    |> Array.zip indices

let private printData (steps: int array) =
    let mapper (i: int) (nextState: int) =
        if i = nextState - 1 || i = nextState || i = nextState + 1 then
            '#'
        else
            '.'
    
    Array.mapi mapper steps
    |> System.String

let private part1 () =
    let data = Data.load phase today 1
    data
    |> Array.choose parseLine
    |> Array.fold compute (1, [| 1 |])
    |> snd
    |> sample 20 40
    |> Array.fold (fun sum (x, y) -> sum + (x + 1) * y) 0

let private part2 () =
    let data = Data.load phase today 1
    data
    |> Array.choose parseLine
    |> Array.fold compute (1, [| 1 |])
    |> snd
    |> fun a -> Array.removeAt (Array.length a - 1) a // superfluous last element
    |> Array.chunkBySize 40
    |> Array.map printData
    |> String.concat "\n"

let run () =
    part1 () |> printfn "TODO: \"%A\""
    part2 () |> printfn "TODO: \n%s"