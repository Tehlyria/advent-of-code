open Utils

let partOne inputList = List.sum inputList

let partTwo inputList =
    let mutable visitedSet = Set.empty.Add(0)
    let mutable result = 0

    let mutable continueLoop = true
    while continueLoop do
        inputList
        |> List.iter (fun frequencyChange ->
            if continueLoop then
                result <- result + frequencyChange
                continueLoop <- not <| Set.contains result visitedSet
                visitedSet <- visitedSet.Add(result)
        )

    result


[<EntryPoint>]
let main argv =
#if INTERACTIVE
    let path = __SOURCE_DIRECTORY__ + "/input.txt"
#else
    let path = "input.txt"
#endif
    let inputList = Utility.readFile path |> List.ofSeq |> List.map (int)
    printf "Part One: %d\n" (partOne inputList)
    printf "Part Two: %d\n" (partTwo inputList)
    0