module ``Day03``

let readFile (filePath: string) = seq {
    use sr = new System.IO.StreamReader(filePath)
    while not sr.EndOfStream do
        yield sr.ReadLine()
}


let (|Regex|_|) pattern inp =
    let m = System.Text.RegularExpressions.Regex.Match(inp, pattern)
    if m.Success then 
        Some (List.tail [ for g in m.Groups -> g.Value ])
    else
        None

type Element =
    Node of int * int * int
    
let coords h w = seq {
    for y in 0 .. h - 1 do 
        for x in 0 .. w - 1 do 
            yield (x, y)
}

let parseLines inp st =
    match inp with 
    | Regex @"#([0-9]+)\s@\s([0-9]+),([0-9]+):\s+([0-9]+)x([0-9]+)" [id; left; top; width; height] ->
        let id = int id
        let left = int left
        let top = int top
        let width = int width
        let height = int height
        
        coords height width
        |> Seq.fold (fun acc (x, y) ->
            let x = x + left
            let y = y + top
            match Map.tryFind (x, y) acc with 
            | Some (elem, ids) -> 
                Map.add (x, y) (elem + 1, id :: ids) acc                
            | None -> 
                Map.add (x, y) (0, [id]) acc
        ) st
    | _ -> 
        st
        
        
let partOne inp =
    inp
    |> Map.filter (fun _ v -> fst v > 0)
    |> Map.count      

let partTwo inp =
    let lst = inp
            |> Map.toList 
            |> List.map (fun elem -> snd elem)
            |> List.groupBy (fun elem -> snd elem)
    
    seq { 1 .. Seq.length inp }
    |> Seq.filter (fun e ->
        lst
        |> List.filter (fun (k, _) -> List.length k > 1)
        |> List.exists (fun (k, _) -> List.contains e k)
        |> not
    )
    |> Seq.head
            
                


[<EntryPoint>]
let main argv =
#if INTERACTIVE
    let path = __SOURCE_DIRECTORY__ + "/input.txt"
#else
    let path = "input.txt"
#endif
    let inp = readFile path |> Seq.fold (fun acc elem -> parseLines elem acc) Map.empty
    inp |> partOne |> printf "Part One: %d\n"
    inp |> partTwo |> printf "Part Two: %d\n"
    0