open System.IO

let filename = "..\..\..\input.txt"

type Grid = char list list

let inputStackHeight = 8
let inputStackWidth = 9

// Parsing functions
let parseCrates (input:List<string>) =    
    let grid =
        List.take inputStackHeight input
    
    let addToList acc item =
        match item with
        | ' ' -> acc
        | _   -> item::acc
    
    // Every 4th value, if it exists, is the letter of a crate
    let rec getColumn (acc:char list) (row:int) (col:int) =
        match row with
        | 0 -> addToList acc grid.[row].[col*4+1]
        | _ -> getColumn (addToList acc grid.[row].[col*4+1]) (row-1) col

    let rec getRows acc col =
        match col with
        | 0 -> (getColumn [] (inputStackHeight-1) col)::acc
        | _ -> getRows ((getColumn [] (inputStackHeight-1) col)::acc) (col-1)

    getRows [] (inputStackWidth-1)

// TryInt validator
let (|Int|_|) (s:string) = 
    try 
        s |> int |> Some
    with :? System.FormatException -> 
        None

// (to, from, amount)
let parseInstruction (instruction:string) =
    let rec parse (arr:List<int>) (str:string) =
        match str with
        | Int i -> i::arr
        | _ -> arr

    let retuple (list:List<'a>) = 
        (list[0]-1, list[1]-1, list[2])

    instruction.Split(' ')
    |> List.ofArray
    |> List.fold parse []
    |> retuple

// Returns the grid with one crate moved
let modGrid (grid:Grid) to_ from =
    let poppedCrate = List.tryHead grid[from]
    
    grid
    |> List.mapi (fun i col -> if i = from then List.tail col else col)
    |> List.mapi (fun i col -> if i = to_ then poppedCrate.Value::col else col)
   
// Applies an instruction tuple to move crates one at a time
let rec applyInstruction (grid:Grid) instruction =
    let rec stack grid inst =
        match inst with
        | (to_, from, 1) -> modGrid grid to_ from
        | (to_, from, amount) -> stack (modGrid grid to_ from) (to_, from, amount-1)

    stack grid instruction

// Returns the grid after moving a substack of crates at once
let bulkModGrid (grid:Grid) to_ from amount =
    let poppedCrates = List.take amount grid[from]

    grid
    |> List.mapi (fun i col -> if i = from then col[amount..] else col)
    |> List.mapi (fun i col -> if i = to_ then poppedCrates @ col else col)

// Apply instructions to move stacks of crates at once. Written to be List.fold'ed
let rec applyInstruction2 (grid:Grid) instruction =
    bulkModGrid grid <||| instruction 


let items =
    filename
    |> File.ReadAllLines
    |> List.ofArray

let crates = parseCrates items

let parsedInstructions = items[inputStackHeight+2..] |> List.map parseInstruction
    
parsedInstructions
|> List.fold applyInstruction crates
|> List.map List.head
|> Array.ofList
|> System.String.Concat
|> printfn "Top Crates (CrateMover 9000): %s"

parsedInstructions
|> List.fold applyInstruction2 crates
|> List.map List.head
|> Array.ofList
|> System.String.Concat
|> printfn "Top Crates (CrateMover 9001): %s"
