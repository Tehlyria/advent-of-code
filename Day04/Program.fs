open Utils
        
// ID * Month * Day * Hour * Minute
type Event =
    | ShiftStart    of int * int * int * int * int
    | FallAsleep    of int * int * int * int * int 
    | WakeUp        of int * int * int * int * int 
    
// ID * Month * Day * H_Start * M_Start * Duration    
type Sleep = SleepDuration of int * int * int * int * int * int

let parseLines = function
    | Utility.Regex @"\[[0-9]{4}\-([0-9]{2})\-([0-9]{2})\s+([0-9]{2}):([0-9]{2})\]\s.*#([0-9]+)" [month; day; hour; minute; id] ->
        ShiftStart(int id, int month, int day, int hour, int minute)
    | Utility.Regex @"\[[0-9]{4}\-([0-9]{2})\-([0-9]{2})\s+([0-9]{2}):([0-9]{2})\]\s.*falls" [month; day; hour; minute] ->
        FallAsleep(0, int month, int day, int hour, int minute)
    | Utility.Regex @"\[[0-9]{4}\-([0-9]{2})\-([0-9]{2})\s+([0-9]{2}):([0-9]{2})\]\s.*wakes" [month; day; hour; minute] ->
        WakeUp(0, int month, int day, int hour, int minute)
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
        let totalDuration = snd elem
                            |> List.sumBy (fun elem -> 
                                match elem with
                                | SleepDuration(_, _, _, _, _, d) -> d
                            )
        (fst elem, totalDuration)
    )
    |> List.maxBy (snd)
    
    
let mapToMinute (acc: Map<int, int>) elem = 
    let (SleepDuration(_, _, _, _, M, _)) = elem
    match Map.tryFind M acc with 
    | Some v -> Map.add M (v + 1) acc
    | None -> Map.add M 1 acc
    
let expandSleepDuration acc elem =
    let (SleepDuration(id, m, d, H, M, D)) = elem
        
    seq { M .. M + D }        
    |> Seq.fold (fun a e -> SleepDuration(id, m, d, H, e, D) :: a) acc              
        
let mostSleptMinute guardID sleepDurs =
    sleepDurs
    |> List.filter (fun elem -> 
        let (SleepDuration(id, _, _, _, _, _)) = elem
        id = guardID
    )
    |> List.fold (expandSleepDuration) []
    |> List.fold (mapToMinute) Map.empty
    |> Map.toList
    |> List.maxBy (snd)
    
let getAllEvents inp =    
    let mutable currentID = -1
    inp
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
            
let partOne inp =
    let sleepDurs = inp |> getAllEvents |> getSleepDurations
    let sleepiestGuard = sleepDurs |> findSleepiestGuard |> fst
    
    mostSleptMinute sleepiestGuard sleepDurs 
    |> (fun m -> sleepiestGuard * fst m)
    
let partTwo inp =
    let sleepDurs = inp |> getAllEvents |> getSleepDurations
    sleepDurs
    |> List.map (fun elem -> 
        let (SleepDuration(id, _, _, _, _, _)) = elem
        id
    )
    |> List.fold (fun acc id ->
        let m = mostSleptMinute id sleepDurs
        (id, m) :: acc
    ) []        
    |> List.sortByDescending (snd << snd)
    |> List.distinct
    |> List.map (fun elem -> fst elem * (fst << snd) elem)       
    |> List.head

[<EntryPoint>]
let main argv =
#if INTERACTIVE
    let path = __SOURCE_DIRECTORY__ + "/input.txt"
#else
    let path = "input.txt"
#endif
    let inp = Utility.readFile path |> Seq.sort |> Seq.toList
    inp |> partOne |> printf "Part One: %d\n"
    inp |> partTwo |> printf "Part Two: %d\n"
    0