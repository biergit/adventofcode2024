open System.IO

let lines = File.ReadAllLines "input.txt"
let sortedNumbers = lines |> Array.map (fun line -> line.Split("   "))
                    |> Array.map (fun parts -> Array.map int parts)
                    |> Array.map (fun parts -> (parts.[0], parts.[1]))
                    |> Array.unzip
                    |> fun (a, b) -> (Array.sort a, Array.sort b)
                    |> fun (a, b) -> Array.zip a b
printfn "Total distance: %d" (sortedNumbers |> Array.map (fun (a, b) -> abs (a - b)) |> Array.sum)
 
let similarityScore = 
    sortedNumbers |> Array.unzip 
            |> fun (a, b) 
                -> (a, (b |> Array.countBy id) |> Map.ofArray)
                |> fun (a, b) 
                    -> a |> Array.map (fun number -> (number, Map.tryFind number b |> Option.defaultValue 0))
                         |> Array.map (fun (a, b) -> a*b) |> Array.sum

printfn "Similarity: %d" similarityScore
 
 

