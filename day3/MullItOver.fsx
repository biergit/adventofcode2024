open System.IO
open System.Text.RegularExpressions
let program = File.ReadAllText "input.txt"
let mulExpr = "mul\((\d{1,3}),(\d{1,3})\)"
let mulRegex = System.Text.RegularExpressions.Regex(mulExpr)
let sumOfMultiplication input=
    let m = mulRegex.Matches(input)
    let multiplicands = 
        m
        |> Seq.cast<Match> 
        |> Seq.map (fun x -> int x.Groups.[1].Value, int x.Groups.[2].Value)
    let multiplicationResults = multiplicands |> Seq.map (fun (x,y) -> x * y)
    multiplicationResults |> Seq.sum
printfn "Sum of multiplication results: %d" (sumOfMultiplication program)

// Part 2 - Conditional multiplication

let doCon = "do()"
let dontCon = "don't()"
let doParts = program.Split(doCon)
printfn "Do parts: %A" doParts
let partUntilDont (part: string) =
    let dontIndex = part.IndexOf(dontCon)
    if dontIndex = -1 then part
    else part.Substring(0, dontIndex)
let sumOfConditionalMultiplication = doParts |> Array.map partUntilDont |> String.concat "" |> sumOfMultiplication
printfn "Sum of conditional multiplication results: %d" sumOfConditionalMultiplication
