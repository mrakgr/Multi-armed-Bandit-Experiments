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
