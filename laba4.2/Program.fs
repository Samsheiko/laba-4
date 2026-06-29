open System // Подключаем базовое системное пространство имен

// 1. Обычное бинарное дерево (из вашего первого примера)
type Tree =
    | Empty
    | Node of float * Tree * Tree

// Рекурсивная генерация случайного дерева заданной глубины (из вашего первого примера)
let rec GenerateTree depth (rand: Random) =
    if depth <= 0 then Empty
    else
        let value = rand.NextDouble() * 20.0 - 10.0
        Node(value, GenerateTree (depth - 1) rand, GenerateTree (depth - 1) rand)

// Универсальная функция свертки (Fold) для обхода дерева
let rec FoldTree folder acc tree =
    match tree with
    | Empty -> acc
    | Node(v, l, r) ->
        let accLeft = FoldTree folder acc l
        let accCurrent = folder accLeft v
        FoldTree folder accCurrent r

//проверяет только те цифры, которые видны на экране
let ContainsDigit (digit: char) (value: float) =
    // Формат "F2" принудительно округляет число до 2 знаков после запятой (как при выводе)
    let strValue = value.ToString("F2")
    strValue.Contains(digit)


// Подсчет элементов с цифрой на основе FoldTree
let CountElementsWithDigit digit tree =
    let folder acc v =
        if ContainsDigit digit v then acc + 1 else acc
    FoldTree folder 0 tree

// Красивый графический вывод структуры дерева в консоль (влево и вправо)
let rec PrintVisual indent isRight tree =
    match tree with
    | Empty -> ()
    | Node(v, l, r) ->
        let rightIndent = indent + (if isRight then "        " else "│       ")//если тру, то мы выводим вправо
        PrintVisual rightIndent true r
        
        printf "%s" indent
        if indent = "" then printf "Root ── "
        elif isRight then printf "┌──(R)─ "
        else printf "└──(L)─ "
        printfn "%0.2f" v
        
        let leftIndent = indent + (if isRight then "│       " else "        ")
        PrintVisual leftIndent false l

[<EntryPoint>]
let main _ =
    printf "Задайте глубину дерева: "
    let depth = Console.ReadLine() |> int
    
    let rand = Random()
    // Генерируем обычное дерево по вашей первой логике
    let myTree = GenerateTree depth rand
    
    printfn "\n=== СТРУКТУРА СЛУЧАЙНОГО ДЕРЕВА ==="
    PrintVisual "" false myTree
    
    printf "\nКакую цифру искать в числах (0-9)? "
    // Считываем строку и берем самый первый символ (char)
    let digit = Console.ReadLine().[0]
    
    // Запускаем подсчет
    let result = CountElementsWithDigit digit myTree
    
    printfn "\nКоличество элементов, содержащих цифру '%c': %d" digit result
    0
