open System

type Tree =
    | Empty
    | Node of float * Tree * Tree

let rec generateTree depth (rand: Random) =
    if depth <= 0 then 
        Empty
    else
        let value = rand.NextDouble() * 20.0 - 10.0
        Node(value, generateTree (depth - 1) rand, generateTree (depth - 1) rand)

let rec foldTree folder acc tree =
    match tree with
    | Empty -> acc
    | Node(v, l, r) ->
        let accLeft = foldTree folder acc l
        let accCurrent = folder accLeft v
        foldTree folder accCurrent r

let containsDigit (digit: char) (value: float) =
    let strValue = value.ToString("F2")
    strValue.Contains(digit)

let countElementsWithDigit digit tree =
    let folder acc v =
        if containsDigit digit v then acc + 1 else acc
    foldTree folder 0 tree

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
    printf "Задайте глубину дерева: "
    let depth = Console.ReadLine() |> int
    
    let rand = Random()
    let myTree = generateTree depth rand
    
    printfn "\n=== СТРУКТУРА СЛУЧАЙНОГО ДЕРЕВА ==="
    printVisual "" false myTree
    
    printf "\nКакую цифру искать в числах (0-9)? "
    let digit = Console.ReadLine().[0]
    
    let result = countElementsWithDigit digit myTree
    
    printfn "\nКоличество элементов, содержащих цифру '%c': %d" digit result
    0
