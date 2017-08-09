open System.Collections.Generic

type Ty =
    | Int
    | Tuple of Ty list
    | Rec of Lazy<Ty>

let rec int_list = Tuple [Int; Rec (lazy int_list)] 

let d = Dictionary()
d.[int_list] <- 3
    
d.[int_list] // 3