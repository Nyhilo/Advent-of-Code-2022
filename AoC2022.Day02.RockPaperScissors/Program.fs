open System.IO

let filename = "..\..\..\input.txt"


let rec calcScore totalScore str =
    match str with
    | "A X" -> (totalScore + 3+1) // Rock ties Rock
    | "A Y" -> (totalScore + 6+2) // Paper beats Rock
    | "A Z" -> (totalScore + 0+3) // Scissors loses to Rock
    | "B X" -> (totalScore + 0+1) // Rock loses to Paper
    | "B Y" -> (totalScore + 3+2) // Paper ties Paper
    | "B Z" -> (totalScore + 6+3) // Scissors beats Paper
    | "C X" -> (totalScore + 6+1) // Rock beats Scissors
    | "C Y" -> (totalScore + 0+2) // Paper loses to Scissors
    | "C Z" -> (totalScore + 3+3) // Scissors ties Scissors
    | _     -> totalScore

filename
|> File.ReadAllLines
|> List.ofArray
|> List.fold calcScore 0
|> printfn "%i"


let rec calcScore2 totalScore str =
    match str with
    | "A X" -> (totalScore + 3) // Scissors loses
    | "A Y" -> (totalScore + 4) // Rock ties
    | "A Z" -> (totalScore + 8) // Paper wins
    | "B X" -> (totalScore + 1) // Rock loses
    | "B Y" -> (totalScore + 5) // Paper ties
    | "B Z" -> (totalScore + 9) // Scissors wins
    | "C X" -> (totalScore + 2) // Paper loses
    | "C Y" -> (totalScore + 6) // Scissors ties
    | "C Z" -> (totalScore + 7) // Rock wins   
    | _     -> totalScore

filename
|> File.ReadAllLines
|> List.ofArray
|> List.fold calcScore2 0
|> printfn "%i"




