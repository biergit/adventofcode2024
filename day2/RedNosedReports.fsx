open System.IO
let reports = File.ReadAllLines "input.txt"
let reportsAsInts = reports |> Array.map (fun report -> report.Split(" ") |> Array.map int |> List.ofArray)
printfn "ReportsAsInts: %A" reportsAsInts
let safe report = 
    let rec checkSafety (report, previous, isDescending, index)  = 
        match report with
        | [] -> (true, index)
        | x::xs -> 
            if isDescending then 
                match x < previous && previous - x <= 3 with
                | true -> checkSafety (xs, x, isDescending, index + 1)
                | _ -> (false, index)
            else 
                match x > previous && x - previous <= 3 with
                | true -> checkSafety (xs, x, isDescending, index + 1)
                | _ -> (false, index)
    let safetyResult =
        // Match the report list to handle different cases
        match report with
            // If the report is empty or has only one element, it's safe
            | [] | [_] -> (true, 0)
            // If the report has more than one element, check the first two elements
            | x::y::zs -> 
                if x > y && x - y <= 3 then checkSafety(zs, y, true, 2)
                // If the first element is less than the second and the difference is within 3, check the rest in ascending order
                else if x < y && y - x <= 3 then checkSafety(zs, y, false, 2)
                // If neither condition is met, the report is not safe
                else (false,1)
    safetyResult
let reportSafetyResults = Array.zip reportsAsInts (reportsAsInts |> Array.map safe)
let safeReportsAsInts = 
    reportSafetyResults 
    |> Array.choose (fun (report, (isSafe, _)) -> if isSafe then Some report else None)
let numberOfSafeReports = safeReportsAsInts |> Array.length
printfn "Safe reports: %d" numberOfSafeReports
let safeReportLines = safeReportsAsInts |> Array.map (fun report ->  report |> List.map string |> String.concat " ")
let unsafeReportLines = reports |> Set.ofArray |> Set.difference (Set.ofArray safeReportLines)
printfn "Unsafe reports: %A" unsafeReportLines
// File.WriteAllLines("unsafeReports.txt", unsafeReportLines)

// Part 2 - Dampener

let dampen (report, violationIndex) =
    let indexedReport= report |> List.mapi (fun i level -> (i, level))
    printfn "Dampening report: %A" report
    let returnValue = 
        let rec checkSafetyWithoutElement violationIndex offset =
            if (violationIndex >= offset && offset < 3) then
                printfn "Checking safety without element %d" offset
                let isSafeWithoutElement = indexedReport |> List.removeAt (violationIndex - offset) |> List.map snd |> safe |> fst
                if isSafeWithoutElement then true
                else checkSafetyWithoutElement violationIndex (offset + 1)
            else false
        checkSafetyWithoutElement violationIndex 0
    returnValue

let isSafeAfterDampeningBruteForce (unsafeReport: list<int>) =
    unsafeReport
    |> List.mapi (fun index level -> index)
    |> List.exists (fun index -> fst(safe (List.removeAt index unsafeReport)))

let unsafeReports = 
    reportSafetyResults 
    |> Array.choose (fun (report, (isSafe, index)) -> if not isSafe then Some (report, index) else None)
let safeReportsAfterDampening = unsafeReports |>  Array.filter dampen |> Array.map fst
let  safeReportsAfterDampeningBruteForce= unsafeReports |> Array.map fst |> Array.filter isSafeAfterDampeningBruteForce
printfn "Safe reports after dampening brute force: %d" (safeReportsAfterDampeningBruteForce |> Array.length)
let numberOfSafeReportsAfterDampening = safeReportsAfterDampening |> Array.length
printfn "Safe reports after dampening: %d" numberOfSafeReportsAfterDampening
let safeReportsDifferent = Set.difference (Set.ofArray safeReportsAfterDampeningBruteForce) (Set.ofArray safeReportsAfterDampening)
printfn "Safe reports after dampening different: %A" safeReportsDifferent
let safeReportsAfterDampeningLines = safeReportsAfterDampening |> Array.map (fun report ->  report |> List.map string |> String.concat " ")
let unsafeReportsAfterDampening = unsafeReports |> Array.map fst |> Set.ofArray |> Set.difference (Set.ofArray safeReportsAfterDampening)
printfn "Unsafe reports after dampening: %A" unsafeReportsAfterDampening
printfn "All safe reports: %A" (safeReportsAfterDampeningLines |> Array.append safeReportLines)
printfn "Number of safe reports: %d" safeReportLines.Length
printfn "Number of all safe reports: %d" ((safeReportsAfterDampeningLines |> Array.append safeReportLines) |> Array.length)