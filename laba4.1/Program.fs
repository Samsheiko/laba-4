open System

type Tree =
    | Empty
    | Node of float * Tree * Tree

let rec Insert value tree =
    match tree with
    | Empty -> Node(value, Empty, Empty)
    | Node(v, l, r) ->  
        if value < v then //условия если корень меньше влево, если больше вправо
            Node(v, Insert value l, r)
        else 
            Node(v, l, Insert value r)

let rec Transform tree = //Функция для замены полож и отриц чисел,  изменение у узла деревьев 
    match tree with
    | Empty -> Empty
    | Node(v, l, r) ->
        let newValue = 
            if v > 0.0 then 1.0
            elif v < 0.0 then 0.0
            else v
        Node(newValue, Transform l, Transform r)

let rec PrintInOrder tree = //Функция для вывода элементов деревьев по возрастанию 
    match tree with
    | Empty -> ()
    | Node(v, l, r) ->
        PrintInOrder l // Сначала в левое поддерево
        printf "%0.2f; " v //Корень 
        PrintInOrder r   //Правое поддерево 

[<EntryPoint>]
let main _ = 
    printf "Сколько чисел добавить в дерево поиска? "
    let count = Console.ReadLine() |> int
    
    let rand = Random()
 
    let bstTree = //Поочередное строение бинарного дерева 
        List.init count (fun _ -> rand.NextDouble() * 20.0 - 10.0)
        |> List.fold (fun accTree value -> Insert value accTree) Empty
    
    printf "\nИсходное дерево поиска (вывод по возрастанию): "
    PrintInOrder bstTree
    printfn ""
    
    let transformed = Transform bstTree
    printf "Преобразованное дерево (0 и 1): "
    PrintInOrder transformed
    printfn ""
    0
