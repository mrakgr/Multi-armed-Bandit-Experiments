let tack x xss = (x :: List.head xss) :: List.tail xss

let rec partition f = function
    | [] -> []
    | [x] -> [[x]]
    | (x::x'::xs) ->
        if f x = f x' then tack x (partition f (x'::xs))
        else [x] :: partition f (x'::xs)

let odd x = x % 2 = 1
let t1 = partition odd [1;3;2;4;1]
