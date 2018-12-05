open Utils

let react ((l: char), (r: char)): bool =
    match (l, r) with
    | (a, b) when a = System.Char.ToUpper(b) -> false 
    | (a, b) when b = System.Char.ToUpper(a) -> false
    | _ -> true
  
let addNextChar c =
    if (System.Char.IsUpper(c)) then 
        String.concat "" [(string c); (string (System.Char.ToLower c))]
    else 
        String.concat "" [(string c); (string (System.Char.ToUpper c))]
                

let filterPairs inp =
    seq { 'a' .. 'z' } 
    |> Seq.append (seq { 'A' .. 'Z' })
    |> Seq.map (addNextChar)
    |> Seq.fold (fun (acc: string) elem -> acc.Replace(elem, "")) inp


let rec partOne (inp: string) =
    let result = filterPairs inp
    if (result.Equals(inp)) then 
        result.Length
    else 
        partOne result        
         

[<EntryPoint>]
let main argv =
#if INTERACTIVE
    let path = __SOURCE_DIRECTORY__ + "/input.txt"
#else
    let path = "input.txt"
#endif
    let inp = Utility.readFile path |> Seq.head
    inp |> partOne |> printf "Part One: %d\n"
    0