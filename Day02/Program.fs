open Utils

let hasPairOfSize chSize inp =
    inp
    |> Seq.groupBy (id) 
    |> Seq.map (fun (k, v) -> (k, Seq.length v)) 
    |> Seq.filter (fun (k, v) -> v = chSize) 
    |> Seq.map (fun (k, v) -> v) 
    |> Seq.length > 0

let countDuplicates chSize inputList =
    inputList
    |> Seq.fold (fun acc elem ->
        if hasPairOfSize chSize elem then
            acc + 1
        else
            acc
        ) 0

let partOne inputList =
    let doubles = countDuplicates 2 inputList
    let triples = countDuplicates 3 inputList
    doubles * triples
    
let hamming lhs rhs =
    lhs
    |> Seq.zip (rhs)
    |> Seq.map (fun (l, r) -> if l = r then 0 else 1)
    |> Seq.sum
    
let partTwo inputList =
    let possiblePairs = inputList |> List.allPairs inputList |> List.filter (fun (l, r) -> hamming l r = 1)
    
    let equalChars ((left: string), (right: string)) =
        left
        |> Seq.zip (right)
        |> Seq.filter (fun (l, r) -> l = r)
        |> Seq.map (fun (l, r) -> string l)
    
    String.concat "" <| (
        possiblePairs
        |> List.map equalChars
        |> List.distinct
        |> List.head
    )

[<EntryPoint>]
let main argv =
#if INTERACTIVE
    let path = __SOURCE_DIRECTORY__ + "/input.txt"
#else
    let path = "input.txt"
#endif
    let inputList = Utility.readFile path |> List.ofSeq
    inputList |> partOne |> printf "Part One: %d\n"
    inputList |> partTwo |> printf "Part Two: %s\n"
    0