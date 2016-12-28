//During the past few days, I've been thinking how to do pattern matching using functions...and now I know.
//
//Suppose you represented the Red Black trees or any data structure for that matter, with functions you could simply do:

//    let balance = function                              (* Red nodes in relation to black root *)
//        | B, T(R, T(R, a, x, b), y, c), z, d            (* Left, left *)
//        | B, T(R, a, x, T(R, b, y, c)), z, d            (* Left, right *)
//        | B, a, x, T(R, T(R, b, y, c), z, d)            (* Right, left *)
//        | B, a, x, T(R, b, y, T(R, c, z, d))            (* Right, right *)
//            -> T(R, T(B, a, x, b), y, T(B, c, z, d))
//        | c, l, x, r -> T(c, l, x, r)
//
//…
//
//type color = R | B    
//type 'a tree =
//    | E
//    | T of color * 'a tree * 'a * 'a tree
//
//let T' col a b c en =
//    [a en; b en; c en]
//    |> List.concat
//let v' en = [E] // Returns subnode
//let d' en = [E] // Returns current node
//
//let turn_into_tree (a,x,b,y,c,z,d) =
//    T(R, T(B, a, x, b), y, T(B, c, z, d))
//let balance_left_left = 
//    T' B (T' R (T' R v' d' v') d' v') d' v'            (* Left, left *)
    //|> fun [a;x;b;y;c;z;d] -> turn_into_tree (a,x,b,y,c,z,d)

// Well, close enough. I do not feel like doing it in detail. Just imagine if the tree was a function ('env → 'a), 
// the balance_left_left could return a single function that can be passed into the tree to do pattern matching.
//
// I was busting my head how to do this with functions, but I could get most of the benefits of having 
// look ahead with just functions by defining a smallish DSL for doing pattern matching using vanilla
//
// This gives me confidence that by chosing the purely functional approach, I am not losing the benefits of pattern matching.
// It is in essence like having a minicompiler for pattern matching.
//

// Edit: Possibly the best example of functional pattern matching are parser combinators. It just never occured to me to put 
// DU pattern matching and parser combinators together.

// Pattern matching on datatypes is not a reason to chose DUs over functions, that much is certain.