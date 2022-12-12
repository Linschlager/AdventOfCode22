namespace AdventOfCode

open System.Reflection

type Data =
    | Test
    | Prod

[<RequireQualifiedAccess>]
module Data =
    let load (data: Data) (day: Day) (index: int) : string array =
        let fileName = $"%A{day}%A{data}%i{index}.txt"
        let resourceName = $"AdventOfCode.Data.%s{fileName}";
        use stream = Assembly.GetExecutingAssembly().GetManifestResourceStream resourceName
        use reader = new System.IO.StreamReader (stream)
        let result = reader.ReadToEnd().Split System.Environment.NewLine
        result