module ``Day04``

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
        
// ID * Month * Day * Hour * Minute
type Event =
    | ShiftStart    of int * int * int * int * int
    | FallAsleep    of int * int * int * int * int 
    | WakeUp        of int * int * int * int * int 
    
// ID * Month * Day * H_Start * M_Start * Duration    
type Sleep =    
    | SleepDuration of int * int * int * int * int * int

let parseLines inp =
    match inp with 
    | Regex @"\[[0-9]{4}\-([0-9]{2})\-([0-9]{2})\s+([0-9]{2}):([0-9]{2})\]\s.*#([0-9]+)" [month; day; hour; minute; id] ->
        let month = int month
        let day = int day
        let hour = int hour
        let min = int minute
        let id = int id
        
        ShiftStart(id, month, day, hour, min)
    | Regex @"\[[0-9]{4}\-([0-9]{2})\-([0-9]{2})\s+([0-9]{2}):([0-9]{2})\]\s.*falls" [month; day; hour; minute] ->
        let month = int month
        let day = int day
        let hour = int hour
        let min = int minute
        
        FallAsleep(0, month, day, hour, min)
    | Regex @"\[[0-9]{4}\-([0-9]{2})\-([0-9]{2})\s+([0-9]{2}):([0-9]{2})\]\s.*wakes" [month; day; hour; minute] ->
        let month = int month
        let day = int day
        let hour = int hour
        let min = int minute
        
        WakeUp(0, month, day, hour, min)
    | _ -> 
        failwith "Could not parse line!"

let getSleepDurations inp =
    inp
    |> List.filter (fun elem -> 
        match elem with 
        | ShiftStart(_) -> false 
        | _ -> true 
    )       
    |> List.chunkBySize 2
    |> List.fold (fun acc elem ->
        let (FallAsleep(id, m, d, H, M)) = List.item 0 elem
        let (WakeUp(_, _, _, _, M2)) = List.item 1 elem
        
        SleepDuration(id, m, d, H, M, M2 - M - 1) :: acc
    ) []               
    |> List.rev
            
let findSleepiestGuard inp =
    inp           
    |> List.groupBy (fun elem ->
        match elem with 
        | SleepDuration(id, _, _, _, _, _) -> id
    )
    |> List.map (fun elem ->
        let id = fst elem
        let totalDuration = snd elem
                            |> List.fold (fun acc elem -> 
                                match elem with 
                                | SleepDuration(_, _, _, _, _, d) -> acc + d
                            ) 0
        (id, totalDuration)
    )
    |> List.maxBy (fun elem -> snd elem)
    
    
let mapToMinute (acc: Map<int, int>) elem = 
    let (SleepDuration(_, _, _, _, M, _)) = elem
    match Map.tryFind M acc with 
    | Some v -> Map.add M (v + 1) acc
    | None -> Map.add M 1 acc
    
let expandSleepDuration acc elem =
    let (SleepDuration(id, m, d, H, M, D)) = elem
        
    seq { M .. M + D }        
    |> Seq.fold (fun a e ->
        SleepDuration(id, m, d, H, e, D) :: a        
    ) acc              
        
let partOne inp =
    let mutable currentID = -1
    let events = inp
                    |> List.fold (fun acc elem ->
                        match parseLines elem with 
                        | ShiftStart(id, m, d, H, M) ->
                            currentID <- id
                            ShiftStart(id, m, d, H, M) :: acc
                        | WakeUp(_, m, d, H, M) ->
                            WakeUp(currentID, m, d, H, M) :: acc
                        | FallAsleep(_, m, d, H, M) ->
                            FallAsleep(currentID, m, d, H, M) :: acc           
                    ) []
                    |> List.rev
                
    let sleepDurs = events |> getSleepDurations
    
    let sleepiestGuard = sleepDurs |> findSleepiestGuard |> fst
    sleepDurs
    |> List.filter (fun elem -> 
        match elem with 
        | SleepDuration(id, _, _, _, _, _) when id = sleepiestGuard -> true
        | _ -> false 
    )
    |> List.fold (expandSleepDuration) []
    |> List.fold (mapToMinute) Map.empty
    |> Map.toList
    |> List.maxBy (snd)
    |> (fun m -> sleepiestGuard * fst m)
    
let partTwo inp =
    0    

[<EntryPoint>]
let main argv =
#if INTERACTIVE
    let path = __SOURCE_DIRECTORY__ + "/input.txt"
#else
    let path = "input.txt"
#endif
    let inp = readFile path |> Seq.sort |> Seq.toList
    inp |> partOne |> printf "Part One: %d\n"
    inp |> partTwo |> printf "Part Two: %d\n"
    0 // return an integer exit code
