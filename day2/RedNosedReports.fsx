open System.IO
let reports = File.ReadAllLines "input.txt"
let reportsAsInts = reports |> Array.map (fun report -> report.Split(" ") |> Array.map int |> List.ofArray)
printfn "ReportsAsInts: %A" reportsAsInts
let safe report = 
    let rec loopSafe report previous desc  = 
        match report with
        | [] -> true
        | x::xs -> 
            if desc then 
                if x < previous && previous - x <=3 then loopSafe xs x desc
                else false
            else 
                if x > previous && x - previous <=3 then loopSafe xs x desc
                else false
    match report with
        | [] | [_]-> true
        | x::y::zs -> 
            if x > y && x - y <= 3 then loopSafe zs y true
            else if x < y && y - x <= 3 then loopSafe zs y false
            else false
let safeReports = reportsAsInts |> Array.filter safe |> Array.length
printfn "Safe reports: %d" safeReports
    