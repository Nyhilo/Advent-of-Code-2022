open System.IO

let filename = "..\..\..\input.txt"


let fullOverlap (a1:int) (a2:int) (b1:int) (b2:int) =
    (a1 >= b1 && a2 <= b2) || (b1 >= a1 && b2 <= a2)

let partialOverlap (a1:int) (a2:int) (b1:int) (b2:int) =
    (a1 <= b2 && a1 >= b1) || (b1 <= a2 && b1 >= a1) ||
    (a2 <= b2 && a2 >= b1) || (b2 <= a2 && b2 >= a1)

let parse overlap (str:string) =
    let split (s:string) =
        s.Split '-'
        |> Array.map int

    let a =
        str.Split ','
        |> Array.map split

    overlap a.[0].[0] a.[0].[1] a.[1].[0] a.[1].[1]

let count (l:List<'a>) =
    l.Length

let items =
    filename
    |> File.ReadAllLines
    |> List.ofArray

items
|> List.map (parse fullOverlap)
|> List.filter ((=) true)
|> count
|> printfn "Total fully overlapping jobs: %i"

items
|> List.map (parse partialOverlap)
|> List.filter ((=) true)
|> count
|> printfn "Total overlapping jobs: %i"