open System.IO

let list: list<int> = [100000; 3; 342; 230; 123; 3849; 29];

let rec filter (pred: 'a -> bool) (list: list<'a>): list<'a> =
    match list with
    | [] -> []
    | head :: rest ->
        if pred head then
            head :: filter pred rest
        else
            filter pred rest

let tryMax (list: list<int>): Option<int> = 
    match list with 
    | [] -> None
    | head :: rest ->
        let max a b : int = if a > b then a else b
        let listMax = List.fold max 0
        Some(listMax list)

open System.IO

type fileLine = { content : string; number : int; fileName : string}

let readFile (file : string) : list<fileLine> = 
    File.ReadAllLines(file) 
    |> List.ofArray 
    |> List.mapi(fun i x -> {content = x; number = i + 1; fileName = file}) 

let lineContainsSearch (line: string) (search : string) = line.Contains(search)

let simpleGrep (search : list<string>)(files : list<string>) = // : list<string> =
    let allLines = List.map(fun x -> readFile x) files
    printfn "%A" allLines



// let array = readFile "ipsum.txt"
// printfn "%A" array

// let findStringMatch (file : string) (search : string): list<string> = 
//     let list = []
//     let fileContent: string array = readFile file
    // List.map (fun s -> lineContainsSearch s)



// let searchFile (search : list<string>) (file: string): list<string> =
//     List.choose 

// let filterByOdd: list<int> = filter (fun c -> c % 2 = 1) list
// let getMax: Option<int> = tryMax list
