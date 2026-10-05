open System

type Tree =
    | Empty                          
    | Node of float * Tree * Tree    

let rnd = Random()

let random minNumber maxNumber = 
    minNumber + (maxNumber - minNumber) * rnd.NextDouble()

let rec insert number tree =
    match tree with
    | Empty -> Node(number, Empty, Empty)
    | Node(v, left, right) ->
        if number < v then
            Node(v, insert number left, right)
        else
            Node(v, left, insert number right)

let rec generate nodeCount minNumber maxNumber =
    if nodeCount <= 0 then Empty
    else
        let number = random minNumber maxNumber
        insert number (generate (nodeCount - 1) minNumber maxNumber)

let rec maptree f tree =
    match tree with
    | Empty -> Empty
    | Node(v, left, right) ->
        Node(f v, maptree f left, maptree f right)

let rec print indent tree =
    match tree with
    | Empty -> ()
    | Node(v, left, right) ->
        printfn "%s%.2f" indent v
        print (indent + "  ") left
        print (indent + "  ") right

[<EntryPoint>]
let main argv =
    let nodeCount = 8        
    let minNumber = -50.0       
    let maxNumber = 50.0        
    
    let tree = generate nodeCount minNumber maxNumber
    
    printfn "\nДерево до :"
    print "" tree          
   
    let treetwo = maptree (fun v -> if v < 0.0 then 0.0 else 1.0) tree
     
    printfn "\nДерево после (Замена: <0 на 0, >=0 на 1) :"
    print "" treetwo
    
    0
