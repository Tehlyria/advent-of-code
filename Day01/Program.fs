module ``Day01``

open System
open System.Collections.Generic
open System.IO
open System.Net

let readFile (filePath: string) = seq {
    use sr = new System.IO.StreamReader(filePath)
    while not sr.EndOfStream do
        yield sr.ReadLine()
}

let partOne (inputList: int list): int = List.sum inputList

let partTwo (inputList: int list): int =
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
    let inputList = readFile path |> List.ofSeq |> List.map (int)
    printf "Part One: %d\n" (partOne inputList)
    printf "Part Two: %d\n" (partTwo inputList)
    0