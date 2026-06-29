open System

type Tree =
    | Empty                             
    | Node of float * Tree * Tree       

let rec Insert value tree =
    match tree with                     
    | Empty -> Node(value, Empty, Empty) 
    | Node(v, l, r) ->                  
        if value < v then               
            Node(v, Insert value l, r)  
        else                            
            Node(v, l, Insert value r)  

let rec Transform tree =
    match tree with                     
    | Empty -> Empty                    
    | Node(v, l, r) ->                  
        let newValue =                  
            if v > 0.0 then 1.0         
            elif v < 0.0 then 0.0       
            else v                      
        Node(newValue, Transform l, Transform r) 

// indent — текущий отступ для выравнивания уровней
// isRight — флаг, показывающий, является ли ветка правой, чтобы рисовать верхние или нижние стрелки
let rec PrintVisual indent isRight tree =
    match tree with
    | Empty -> () // Пустые узлы не выводим, чтобы не перегружать экран графикой
    | Node(v, l, r) ->
        // Сначала рекурсивно выводим правое поддерево 
        let rightIndent = indent + (if isRight then "        " else "│       ")
        //Если мы в правой ветке, то идем вправо, если в левой, то вниз
        PrintVisual rightIndent true r
        
        //Выводим текущий узел с красивым префиксом направления
        printf "%s" indent
        if indent = "" then
            printf "Root ── " // корень дерева
        elif isRight then
            printf "┌──(R)─ " // правая ветка (вверху)
        else
            printf "└──(L)─ " // левая ветка (внизу)
            
        printfn "%0.2f" v // печатаем само число
        
        // Затем рекурсивно выводим левое поддерево (оно будет отображаться внизу)
        let leftIndent = indent + (if isRight then "│       " else "        ")//Выводим левую ветку 
        PrintVisual leftIndent false l

[<EntryPoint>]                          
let main _ =                            
    printf "Сколько чисел добавить в дерево поиска? " 
    let count = Console.ReadLine() |> int 
    
    let rand = Random()                 
    
    let bstTree = 
        List.init count (fun _ -> rand.NextDouble() * 20.0 - 10.0) 
        |> List.fold (fun accTree value -> Insert value accTree) Empty 
    
    printfn "\n=== СТРУКТУРА ИСХОДНОГО ДЕРЕВА ===" 
    PrintVisual "" false bstTree                
    
    let transformed = Transform bstTree 
    printfn "\n=== СТРУКТУРА ПОСЛЕ ЗАМЕНЫ (0 и 1) ===" 
    PrintVisual "" false transformed            
    0                                   
