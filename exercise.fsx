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

type fileLine = { content : string; lineNumber : int; fileName : string}

let readFile (file : string) : list<fileLine> = 
    File.ReadAllLines(file) 
    |> List.ofArray 
    |> List.mapi(fun i x -> {content = x; lineNumber = i + 1; fileName = file}) 

let lineContainsAnySearch (line: fileLine) (search: list<string>): bool = List.exists(fun (x: string) -> line.content.Contains(x)) search

let simpleGrep (search : list<string>)(files : list<string>) : list<fileLine> = 
    files 
    |> List.map(fun x -> readFile x)
    |> List.concat
    |> List.where(fun fileLine -> lineContainsAnySearch fileLine search)
 
let grep: list<fileLine> = simpleGrep ["Lorem";"reprehenderit";"Excepteur"] ["lorem.txt"; "ipsum.txt"]
let getMaxOfList: Option<int> = tryMax list
let filterByOdd: list<int> = filter (fun c -> c % 2 = 1) list