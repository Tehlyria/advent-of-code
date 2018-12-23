open System.Reflection.Metadata
open Utils

type Entry = { x: int; y: int; z: int; r: int }

let private parseLines inp =
    let rec impl line lst =
        match line with
        | Utility.Regex @"^pos=<(-?[\d]+),(-?[\d]+),(-?[\d]+)>, r=([\d]+)$" [x; y; z; r] ->
            { x = int x; y = int y; z = int z; r = int r } :: lst
        | _ ->
            failwithf "Could not parse '%s'" line
            
    Seq.fold (fun acc elem -> impl elem acc) [] inp
    
let private manhattanDist src dst =
    let { x = x1; y = y1; z = z1 } = src
    let { x = x2; y = y2; z = z2 } = dst
    abs(x1 - x2) + abs(y1 - y2) + abs(z1 - z2) 
    
        
let private partOne entries =
    let maxRadius = entries |> List.maxBy (fun e -> e.r)
    entries
    |> List.filter (fun e -> manhattanDist maxRadius e <= maxRadius.r)
    |> List.length


[<EntryPoint>]
let main argv =
#if INTERACTIVE
    let path = __SOURCE_DIRECTORY__ + "/input.txt"
#else
    let path = "input.txt"
#endif
    let inp = Utility.readFile path
    inp |> parseLines |> partOne |> printf "%d"
    0