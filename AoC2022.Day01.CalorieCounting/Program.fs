open System.IO

// Function to read lines from input file
let filename = "..\..\..\input.txt"
let readlines file =
    let convertToInt (str:string) =
        match str with
        | "" -> 0
        | _  -> str |> int

    file
    |> File.ReadAllLines
    |> List.ofArray
    |> List.map (fun i -> convertToInt i)


let rec getTotals (lines:list<int>) (index:int) (acc:list<int>) (totals:list<int>) = 
    if index = lines.Length then
        // End of file
        totals
    else if lines[index] = 0 then
        // Append new total to totals list
        let total = List.sum acc
        let newtotals = total::totals
            
        // Reset accumulator
        getTotals lines (index + 1) [] newtotals
    else
        // Increment accumulator
        getTotals lines (index + 1) (lines[index] :: acc) totals


let rec getTotals2 (x::ints) str = 
    match str with
    | "" -> 0::x::ints
    | _  -> (x+(str |> int))::ints

let totals =
    filename
    |> File.ReadAllLines
    |> List.ofArray
    |> List.fold getTotals2 [0]

totals
|> List.max
|> printfn "Top snack: %i"

totals
|> List.sortDescending  // -> [sorted]
|> List.take 3  // List.take(3, sorted) ->  return first 3 elements
|> List.sum
|> printfn "Top 3 snacks: %i"



// The most calories carried is...
// printfn "%i" <| List.max (getTotals lines 0 [] [])



/// parseElf (x:ints) str = case str of
///  "" -> [0] ++ (x:ints)
///  otherwise ->  (x+(read str)):ints