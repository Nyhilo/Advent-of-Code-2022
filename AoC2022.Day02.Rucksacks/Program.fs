open System.IO
open System.Linq

let filename = "..\..\..\input.txt"


let intersect (x:List<char>) (y:List<char>) =
    x.Intersect(y)

let intersectString str1 str2 =
    intersect (Seq.toList str1) (Seq.toList str2)

let priority (c:char) = 
    let ord = int c
    match ord with
    | ord when ord >= 65 && ord <= 90  -> ord - 38
    | ord when ord >= 97 && ord <= 122 -> ord - 96
    | _ -> 0

let getMismatchedItemPriority (str:string) =
    let s = Seq.toList str
    let first = s[..((s.Length / 2)-1)]
    let second = s[(first.Length)..]
    priority <| (intersect first second).FirstOrDefault()

let rec groupElves (sum:int, acc:List<string>) str =
    match acc.Length with
    | 2 -> let c =
                (intersectString acc[0] acc[1] |> intersectString str).FirstOrDefault()
                |> priority
           ((sum + c), [])
    | _ -> (sum, str::acc)

let items =
    filename
    |> File.ReadAllLines
    |> List.ofArray

items
|> List.map getMismatchedItemPriority
|> List.sum
|> printfn "Misplaced Items Sum: %i"

items
|> List.fold groupElves (0, [])
|> fst
|> printfn "Badge Sum: %i" 
