module AdventOfCode.Day5

let private today = Day.Day5
let private phase = Data.Prod

[<RequireQualifiedAccess>]
type ParseItem = Char of char | Empty

type State = char list array

[<RequireQualifiedAccess>]
module State =
    let private parseLine (input: string) : ParseItem array =
        let chars = input.ToCharArray ()
        let numExpected = (chars.Length + 1) / 4        
        Array.init numExpected (fun i ->
            let x = i * 4 + 1
            if System.Char.IsWhiteSpace chars[x] then ParseItem.Empty
            else ParseItem.Char chars[x]
        )
                
    let parse (inputs: string array) : State =
        inputs
        |> Array.take (inputs.Length - 1)
        |> Array.map parseLine
        |> Array.transpose
        |> Array.map (
            Array.choose (function
                | ParseItem.Char x -> Some x
                | ParseItem.Empty -> None
            )
        )
        |> Array.map Array.toList 

type Move = { Amount: int; From: int; To: int }

[<RequireQualifiedAccess>]
module Move =
    let tryParse (input: string) =
        match input with
        | Util.Regex "move (\d+) from (\d+) to (\d+)" [Util.Int num; Util.Int from; Util.Int to'] ->
            // Account for zero-indexed data structure
            Some { Amount = num; From = from - 1 ; To = to' - 1 }
        | _ -> None     
    
    let move (state: State) (move: Move) =        
        let rec moveSingle (i: int) (newState: State) =
            let fromItem, newFromStack =
                match newState[move.From] with
                | head :: tail -> head, tail
                | _ -> failwith "Invalid configuration"
            let newToStack = fromItem::newState[move.To]
            
            let newState =
                newState
                |> Array.updateAt move.From newFromStack
                |> Array.updateAt move.To newToStack                
                
            if i > 1 then                
                moveSingle (i-1) newState
            else
                newState
                
        moveSingle move.Amount state   
    
    let move9001 (state: State) (move: Move) =
        
        let fromItem, newFromStack =
            let head = List.take move.Amount state[move.From]
            let tail = List.skip move.Amount state[move.From]
            head, tail
        let newToStack = List.append fromItem state[move.To]
        
        let newState =
            state
            |> Array.updateAt move.From newFromStack
            |> Array.updateAt move.To newToStack   
        newState   

let private part1 () =
    let data = Data.load phase today 1
    
    let head, tail =
        let splitIndex = Array.findIndex ((=)"") data
        Array.splitAt splitIndex data
    
    let state = State.parse head
    let commands =
        tail
        |> Array.skip 1
        |> Array.choose Move.tryParse
    
    let final = Array.fold Move.move state commands
    
    Array.map (List.head >> string) final
    |> String.concat ""
    
let private part2 () =
    let data = Data.load phase today 1
    
    let head, tail =
        let splitIndex = Array.findIndex ((=)"") data
        Array.splitAt splitIndex data
    
    let state = State.parse head
    let commands =
        tail
        |> Array.skip 1
        |> Array.choose Move.tryParse
    
    let final = Array.fold Move.move9001 state commands
    
    Array.map (List.head >> string) final
    |> String.concat ""

let run () =
    part1 () |> printfn "TODO: \"%s\""
    part2 () |> printfn "TODO: \"%s\""