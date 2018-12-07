open System
open Utils

type ParsedLine = char * char

let parseLines line =
    match line with
    | Utility.Regex @"Step ([A-Z]) must be finished before step ([A-Z]) can begin." [src ; dst] ->
        ParsedLine(char src, char dst)        
    | _ -> failwith "Error parsing line."
    

let (|EmptySet|_|) s = if Set.isEmpty s then Some() else None    
    
let getOrphans lst = lst |> Set.filter (fun (src, dst) -> not <| Set.exists (fun (b, a) -> src = a) lst)
    
let rec topoSort nodes =
    match nodes with
    | EmptySet -> []
    | _  ->
        // next to add are nodes without parent
        let currentElems = nodes |> getOrphans
        
        // take alphabetically smallest
        let nextNode = currentElems |> Set.map (fun (s, d) -> s) |> Set.toList |> List.sort |> List.head
        
        // remove nodes we add from the set
        let toRemove = nodes |> Set.filter (fun (s, d) -> s = nextNode)
        let remainingNodes = Set.difference nodes toRemove 
        
        // append and recurse
        List.append [nextNode] (topoSort remainingNodes)
    

let partOne inp =
    let lines = inp |> Seq.fold (fun acc elem -> parseLines elem :: acc) []
    
    // add (R, _) for nodes that only occur on right side
    // this helps with removing nodes in topoSort
    // otherwise we lose info about nodes only occuring on the right
    let allLines = 
        lines 
        |> List.where (fun (_, d) -> not <| List.exists (fun (b, a) -> d = b) lines)
        |> List.map (fun (s, d) -> d)
        |> List.distinct
        |> List.fold (fun acc elem -> ParsedLine(elem, '_') :: acc) []
        |> List.append lines
    
    allLines |> Set.ofList |> topoSort |> List.toSeq |> Seq.map (string) |> String.concat ""

[<EntryPoint>]
let main argv =
#if INTERACTIVE
    let path = __SOURCE_DIRECTORY__ + "/input.txt"
#else
    let path = "input.txt"
#endif
    let inp = Utility.readFile path
    
    // should be CABDFE for test input
    inp |> partOne |> printf "Part One: %s\n"
    0
