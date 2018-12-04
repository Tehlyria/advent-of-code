open Utils

let hasPairOfSize (chSize: int) (inp: string) =
    inp.ToCharArray() 
    |> Array.toSeq 
    |> Seq.groupBy (id) 
    |> Seq.map (fun (k, v) -> (k, Seq.length v)) 
    |> Seq.filter (fun (k, v) -> v = chSize) 
    |> Seq.map (fun (k, v) -> v) 
    |> Seq.length > 0

let countDuplicates (chSize: int) (inputList: string list) =
    inputList
    |> Seq.fold (fun (acc: int) (elem: string) ->
        if hasPairOfSize chSize elem then
            acc + 1
        else
            acc
        ) 0

let partOne (inputList: string list) =
    let doubles = countDuplicates 2 inputList
    let triples = countDuplicates 3 inputList
    doubles * triples
    
let hamming (lhs: string) (rhs: string) =
    lhs.ToCharArray()
    |> Array.zip (rhs.ToCharArray())
    |> Array.map (fun (l, r) -> if l = r then 0 else 1)
    |> Array.sum
    
let partTwo (inputList: string list) =
    let possiblePairs = inputList |> List.allPairs inputList |> List.filter (fun (l, r) -> hamming l r = 1)
    
    let equalChars ((left: string), (right: string)) =
        left.ToCharArray()
        |> Array.zip (right.ToCharArray())
        |> Array.filter (fun (l, r) -> l = r)
        |> Array.map (fun (l, r) -> string l)
    
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
    
