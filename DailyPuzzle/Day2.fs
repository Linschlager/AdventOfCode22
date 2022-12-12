module AdventOfCode.Day2

let private today = Day.Day2
let private phase = Data.Prod

[<RequireQualifiedAccess>]
type private GameResult = Win | Draw | Loss

[<RequireQualifiedAccess>]
module private GameResult =
    let score =
        function
        | GameResult.Win -> 6
        | GameResult.Draw -> 3
        | GameResult.Loss -> 0
    
    let tryParse =
        function
        | "X" -> Some GameResult.Loss
        | "Y" -> Some GameResult.Draw
        | "Z" -> Some GameResult.Win
        | _ -> None
    
    let (|Result|_|) = tryParse
    
[<RequireQualifiedAccess>]
type private Shape =
    | Rock
    | Paper
    | Scissors

[<RequireQualifiedAccess>]
module private Shape =
    let score =
        function
        | Shape.Rock -> 1
        | Shape.Paper -> 2
        | Shape.Scissors -> 3
    
    let tryParse =
        function
        | "A"
        | "X" -> Some Shape.Rock
        | "B"
        | "Y" -> Some Shape.Paper
        | "C"
        | "Z" -> Some Shape.Scissors
        | _ -> None
    
    let result (self: Shape) (other: Shape) =
        match self, other with
        | Shape.Rock, Shape.Paper
        | Shape.Paper, Shape.Scissors
        | Shape.Scissors, Shape.Rock -> GameResult.Loss
        | Shape.Rock, Shape.Rock
        | Shape.Paper, Shape.Paper
        | Shape.Scissors, Shape.Scissors -> GameResult.Draw
        | Shape.Rock, Shape.Scissors
        | Shape.Scissors, Shape.Paper
        | Shape.Paper, Shape.Rock -> GameResult.Win
    
    let requiredShape (other: Shape) (result: GameResult) =
        match other, result with
        | Shape.Rock, GameResult.Win
        | Shape.Paper, GameResult.Draw
        | Shape.Scissors, GameResult.Loss -> Shape.Paper
        | Shape.Rock, GameResult.Draw
        | Shape.Scissors, GameResult.Win
        | Shape.Paper, GameResult.Loss -> Shape.Rock
        | Shape.Rock, GameResult.Loss
        | Shape.Paper, GameResult.Win
        | Shape.Scissors, GameResult.Draw -> Shape.Scissors
    
    let (|Shape|_|) = tryParse

let private scoreLine (other, self) =
    let res = Shape.result self other
    Shape.score self + GameResult.score res

let private part1 () =    
    let parseLine (input: string) =
        let split = input.Split " "
        match split with
        | [| Shape.Shape other; Shape.Shape self |] ->
            (other, self)
        | _ -> failwith input
        
    let data = Data.load phase today 1
    data
    |> Array.map parseLine
    |> Array.map (fun line -> line, scoreLine line)
    |> Array.sumBy snd

let private part2 () =
    let parseLine (input: string) =
        let split = input.Split " "
        match split with
        | [| Shape.Shape other; GameResult.Result expectedResult |] ->
            let self = Shape.requiredShape other expectedResult
            (other, self)
        | _ -> failwith input
    
    let data = Data.load phase today 1
    data
    |> Array.map parseLine
    |> Array.map (fun line -> line, scoreLine line)
    |> Array.sumBy snd

let run () =
    part1 () |> printfn "Total Score if everything goes as planned: \"%A\""
    part2 () |> printfn "Total Score with special strategy: \"%A\""
    