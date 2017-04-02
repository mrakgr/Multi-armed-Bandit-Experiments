// Rewrite of the Probability Eff example with just functions.

let rec uniform toss = function
    | [] -> 0.0
    | [x: int] -> float x
    | x :: xs ->
        let n = List.length xs + 1
        let p = 1.0 / float n
        toss p <| fun k -> if k then float x else uniform toss xs

let w =
    let toss p k = p * k true + (1.0 - p) * k false
    let uniform = uniform toss
    let x = uniform [1; 2; 3; 4; 5; 6] in
    let y = uniform [1; 2; 3; 4; 5; 6] in
    x + y

// Note: No, I really can't add algebraic effects + handlers to the library.
// That is because its control flow.

// I am going to have to make do with simply extending the language with dynamically scoped functions and variables
// by passing them through the environment.

// That should make the new language expressive enough for my needs.