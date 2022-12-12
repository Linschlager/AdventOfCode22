module AdventOfCode.Day7

let private today = Day.Day7
let private phase = Data.Prod

type ConsoleCommand =
    | ChangeDirectoryUp
    | ChangeDirectory of Name: string
    | List

type ConsoleFile =
    | File of Name: string * Size: int
    | Dir of Name: string

type ConsoleOutput =
    | Command of ConsoleCommand
    | File of ConsoleFile

type FileSystem =
    | Directory of Name: string * FileSystem list
    | File of Name: string * Size: int

module FileSystem =
    let rec size (fs: FileSystem) : int =
        match fs with
        | FileSystem.File (_, size) -> size
        | FileSystem.Directory (_, files) -> List.sumBy size files
    
    let rec flat (fs: FileSystem) : (string * (string * int) list) list =
        match fs with
        | FileSystem.File _ -> []
        | FileSystem.Directory (dir, contents) ->
            let isFile f =
                match f with
                | File _ -> true
                | Directory _ -> false
            
            let files, dirs = List.partition isFile contents
            let fakeFiles =
                dirs
                |> List.map (function
                    | File (name, s) -> name, s
                    | Directory (name, sub) -> name, List.sumBy size sub)
                
            let rawFiles =
                files
                |> List.choose (function
                    | File (fn, size) -> Some (fn, size)
                    | _ -> None)
                |> List.append fakeFiles
                
            (dir, rawFiles) :: List.collect flat dirs
            
let private parseCommand (commandStr: string) =
    match commandStr with
    | "cd .." -> ConsoleCommand.ChangeDirectoryUp
    | Util.Regex "cd (.*)" [ dirName ] -> ConsoleCommand.ChangeDirectory dirName
    | "ls" -> ConsoleCommand.List
    | _ -> failwith $"Unknown command: %s{commandStr}"

let private parseLine (input: string) =
    match input with
    | Util.Regex "\$ (.*)" [ commandStr ] -> parseCommand commandStr |> ConsoleOutput.Command
    | Util.Regex "(\d*) (.*)" [ Util.Int size; fileName ] -> ConsoleOutput.File(ConsoleFile.File(fileName, size))
    | Util.Regex "dir (.*)" [ dirName ] -> ConsoleOutput.File(ConsoleFile.Dir dirName)
    | _ -> failwith $"Unknown input: %s{input}"

let private parseFileSystem (console: ConsoleOutput array) : FileSystem =
    let rec parse (rest: ConsoleOutput list) =
        match rest with
        | head :: tail ->
            match head with
            | ConsoleOutput.Command ConsoleCommand.ChangeDirectoryUp -> [], tail
            | ConsoleOutput.Command (ConsoleCommand.ChangeDirectory name) ->
                let dir, rest = parse tail
                let more, rem = parse rest
                FileSystem.Directory(name, dir) :: more, rem
            | ConsoleOutput.Command ConsoleCommand.List -> parse tail
            | ConsoleOutput.File (ConsoleFile.File (name, size)) ->
                let more, rem = parse tail
                FileSystem.File(name, size) :: more, rem
            | ConsoleOutput.File (ConsoleFile.Dir _name) -> parse tail
        | [] -> [], []

    Array.toList console |> (parse >> fst) |> List.head

let private part1 () =
    let data = Data.load phase today 1
    data
    |> Array.map parseLine
    |> parseFileSystem
    |> FileSystem.flat
    |> List.map (fun (_, files) -> List.sumBy snd files)
    |> List.filter (fun size -> size <= 100_000)
    |> List.sum

let private part2 () =
    let totalSize = 70_000_000
    let minRemSize = 30_000_000
    let data = Data.load phase today 1
    let fs = 
        data
        |> Array.map parseLine
        |> parseFileSystem
    let actualSize = FileSystem.size fs
    let remainingSize = totalSize - actualSize
    let requiredSize = minRemSize - remainingSize
    FileSystem.flat fs
    |> List.map (fun (_, files) -> List.sumBy snd files)
    |> List.filter (fun size -> size >= requiredSize)    
    |> List.sort
    |> List.head

let run () =
    part1 () |> printfn "Total Size of directories up to 100k: \"%A\""
    part2 () |> printfn "Min Required Size to be freed up: \"%A\""
