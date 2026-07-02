open System

type Tree =
    | Empty
    | Node of float * Tree * Tree

let rec insert value tree =
    match tree with
    | Empty -> Node(value, Empty, Empty)
    | Node(v, l, r) ->
        if value < v then
            Node(v, insert value l, r)
        else
            Node(v, l, insert value r)

let rec transform tree =
    match tree with
    | Empty -> Empty
    | Node(v, l, r) ->
        let newValue =
            if v > 0.0 then 1.0
            elif v < 0.0 then 0.0
            else v
        Node(newValue, transform l, transform r)

let rec printVisual indent isRight tree =
    match tree with
    | Empty -> ()
    | Node(v, l, r) ->
        let rightIndent = indent + (if isRight then "        " else "│       ")
        printVisual rightIndent true r
        
        printf "%s" indent
        if indent = "" then
            printf "Root ── "
        elif isRight then
            printf "┌──(R)─ "
        else
            printf "└──(L)─ "
            
        printfn "%0.2f" v
        
        let leftIndent = indent + (if isRight then "│       " else "        ")
        printVisual leftIndent false l

[<EntryPoint>]
let main args =
    printf "Сколько чисел добавить в дерево поиска? "
    let count = Console.ReadLine() |> int
    
    let rand = Random()
    
    let bstTree =
        List.init count (fun _ -> rand.NextDouble() * 20.0 - 10.0)
        |> List.fold (fun accTree value -> insert value accTree) Empty
    
    printfn "\n=== СТРУКТУРА ИСХОДНОГО ДЕРЕВА ==="
    printVisual "" false bstTree
    
    let transformed = transform bstTree
    printfn "\n=== СТРУКТУРА ПОСЛЕ ЗАМЕНЫ (0 и 1) ==="
    printVisual "" false transformed
    0

