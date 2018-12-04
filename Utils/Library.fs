namespace Utils

module Utility =
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
