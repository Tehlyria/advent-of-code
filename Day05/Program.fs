open Utils

let duplicateWithCase c =
    if (System.Char.IsUpper(c)) then 
        String.concat "" [(string c); (string (System.Char.ToLower c))]
    else 
        String.concat "" [(string c); (string (System.Char.ToUpper c))]
                

let filterPairs inp =
    seq { 'a' .. 'z' } 
    |> Seq.append (seq { 'A' .. 'Z' })
    |> Seq.map (duplicateWithCase)
    |> Seq.fold (fun (acc: string) elem -> acc.Replace(elem, "")) inp


let rec fullyReact inp =
    let result = filterPairs inp
    if (result.Equals(inp)) then 
        result.Length
    else 
        fullyReact result        


let partOne = fullyReact


let replaceAndReact (inp: string) (ch: string) =
    inp.Replace(ch.ToLower(), "").Replace(ch.ToUpper(), "")
    |> fullyReact
    
    
let partTwo (inp: string) =
    seq { 'a' .. 'z' }
    |> Seq.map (string)
    |> Seq.map (replaceAndReact inp)
    |> Seq.min
    

[<EntryPoint>]
let main argv =
#if INTERACTIVE
    let path = __SOURCE_DIRECTORY__ + "/input.txt"
#else
    let path = "input.txt"
#endif
    let inp = Utility.readFile path |> Seq.head
    inp |> partOne |> printf "Part One: %d\n"
    inp |> partTwo |> printf "Part Two: %d\n"
    0