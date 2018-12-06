open Utils

type Point = { x: int; y: int ; v: int option }

let manhattanDist (p1: Point) (p2: Point) =
    abs(p1.x - p2.x) + abs(p1.y - p2.y)
    
let parseCoordLine l ln =
    match l with 
    | Utility.Regex @"([0-9]+),\s([0-9]+)" [xS ; yS] ->
        { x = int xS ; y = int yS ; v = Some ln }
    | _ -> failwith "Could not parse line."       

let toCoordList inp = 
    inp 
    |> Seq.mapi (fun idx e -> (idx + 1, e))
    |> Seq.fold (fun acc elem -> (parseCoordLine (snd elem) (fst elem)) :: acc) []
    
let findClosestPoint cur points =
    let minDist = points |> Seq.map (manhattanDist cur) |> Seq.min
    points |> Seq.filter (fun e -> manhattanDist cur e = minDist)
    
let generateCoords width height = seq {
    for y in 0 .. height do 
        for x in 0 .. width do 
            yield { x = x ; y = y ; v = None }
}

let countArea width height (anchors: List<Point>) (sq: seq<Point>) =
    let mutable orig = anchors
    
    sq 
    |> Seq.iter (fun elem ->
        match elem with 
        | { x = ex ; y = ey ; v = ev } ->
            // remove elements which have are closest to a cell at the edges
            if ex = 0 || ey = 0 || ex = width || ey = height then 
                match ev with 
                | Some v -> 
                    orig <- List.filter (fun x -> x.v <> Some v) orig
                | _ -> ()                    
    )
    
    sq 
    |> Seq.filter (fun (e: Point) -> List.exists (fun (x: Point) -> x.v = e.v) orig)
    |> Seq.map (fun (e: Point) -> e.v)
    |> Seq.groupBy (id)
    |> Seq.map (snd >> Seq.length)
    |> Seq.max

let partOne inp =
    let l = inp |> toCoordList
    let width = (l |> List.map (fun elem -> elem.x) |> List.max) + 1
    let height = (l |> List.map (fun elem -> elem.y) |> List.max) + 1
    
    generateCoords width height
    |> Seq.map (fun elem -> 
        let { x = px; y = py; v = pv } = elem
        let closestPt = findClosestPoint elem l
        
        if (Seq.isEmpty closestPt) then failwith "Should never be empty!"
        
        if (Seq.length closestPt > 1) then 
            { x = px ; y = py ; v = None  }
        else 
            let hd = Seq.head closestPt
            { x = px ; y = py ; v = hd.v }
    )
    |> (countArea width height l)
 
[<EntryPoint>]
let main argv =
#if INTERACTIVE
    let path = __SOURCE_DIRECTORY__ + "/input.txt"
#else
    let path = "input.txt"
#endif
    let inp = Utility.readFile path
    inp |> partOne |> printf "\nPart One: %d\n"
    0