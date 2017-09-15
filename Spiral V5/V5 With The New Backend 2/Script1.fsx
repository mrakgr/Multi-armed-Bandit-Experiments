//let test55 =
//    "test55",[array;console;parsing4],"Does the v4 of the (monadic) parsing library work? Birthday Cake Candles problem.",
//    """
////https://www.hackerrank.com/challenges/birthday-cake-candles
//open Console
//open Parsing
//
//inl solution x = 
//    inl f = 
//        inm n = parse_int
//        inm ar = parse_n_array parse_int n 
//        Array.foldl (inl (min,score as s) x ->
//            if x > score then (1,x)
//            elif x = score then (min+1,score)
//            else s
//            ) (dyn (0,mscorlib ."System.Int64" .MinValue)) ar
//        |> fst
//        |> writeline
//        |> succ
//    f x
//
//run_with_unit_ret (readall()) solution
//    """
//
//let test56 =
//    "test56",[array;console;parsing4],"Does the v4 of the (monadic) parsing library work? Diagonal Sum Difference problem.",
//    """
////https://www.hackerrank.com/challenges/diagonal-difference
//open Console
//open Parsing
//
//inl abs x = if x >= 0 then x else -x
//
//inl f =
//    inm n = parse_int
//    inm ar = parse_n_array parse_int (n*n)
//    inl load row col = 
//        inl f x = x >= 0 || x < n
//        assert (f row && f col) "Out of bounds."
//        ar (n * row + col)
//    met rec loop (!dyn i) (d1,d2 as s) =
//        if i < n then loop (i+1) (d1 + load i i, d2 + load i (n-i-1))
//        else s
//        : s
//    inl a,b = loop 0 (0,0)
//    abs (a-b) 
//    |> writeline
//    |> succ
//
//run_with_unit_ret (readall()) f
//        """
//
//let test57 =
//    "test57",[array;console;parsing4],"Speed test for the v4 of the parsing library.",
//    """
////https://www.hackerrank.com/challenges/diagonal-difference
//open Console
//open Parsing
//
//inl abs x = if x >= 0 then x else -x
//
//inl f = 
//    Tuple.repeat 240 parse_int |> tuple
//    |>> (Tuple.foldl (+) 0 >> writeline >> succ)
//    
//run_with_unit_ret (readall()) f
//        """

//let test59 =
//    "test59",[parsing4;console],"Does sprintf work for v4?",
//    """
//inl a = dyn 1
//inl b = dyn 2
//Parsing.sprintf "%i + %i = %i" a b (a+b) |> ignore
//Console.printfn "(%i,%f,%b,%s)" 1 2.0 true "4"
//    """
//
//let test60 =
//    "test60",[array;queue;parsing4;console],"https://www.hackerrank.com/challenges/saveprincess",
//    """
//// A simple dynamic programming problem. It wouldn't be hard to do in F#, but Spiral
//// gives some novel challenges regarding it.
//open Parsing
//
//inl Cell =
//    type
//        .Empty
//        .Princess
//        .Mario
//
//inl empty = pchar 'e' >>% Cell .Empty
//inl princess = pchar 'p' >>% Cell .Princess
//inl mario = pchar '-' >>% Cell .Mario
//
//inl cell = empty <|> princess <|> mario
//
//inl parse_cols n = parse_n_array n cell .>> spaces
//inl parse_rows n = parse_n_array n (parse_cols n)
//
//inl solve n field =
//    inl cur_pos on_succ =
//        met rec loop1 (!dyn row) =
//            met rec loop2 (!dyn col) =
//                if col < n then
//                    match field row col with
//                    | .Mario -> on_succ row col
//                    | _ -> loop2 (col+1)
//                else loop1 (row+1)
//            if row < n then loop2 0
//            else failwith "Mario not found."
//        loop1 0
//    cur_pos <| inl row col ->
//    // init the arrays
//    inl cells_visited = Array.init n (inl _ -> Array.init n false)
//    cells_visited row col <- true
//
//    inl ar = Array.singleton ((row,col), List.empty string)
//
//    met rec loop on_fail on_succ =
//        inl row,col = Queue.dequeue p
//        match ar row col with
//        | .Princess -> on_succ()
//        | _ ->
//            inl up = (row-1,col),"UP"
//            inl down = (row+1,col),"DOWN"
//            inl left = (row,col-1),"LEFT"
//            inl right = (row,col+1),"RIGHT"
//            inl is_valid x = x >= 0 && x < n
//            inl is_in_range (row,col) = is_valid row && is_valid col
//            inl select (p,move) rest =
//                if is_in_range p then 
//
//inl f =
//    inm n = parse_int
//    inm ar = parse_rows n
//
//    """
