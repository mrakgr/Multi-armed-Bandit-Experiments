type Data<'data> = C of 'data

type Op<'a> = 
| Op1 of (Data<string> -> 'a)
| Op2 of (Data<int> -> 'a)

let operate<'a> act:'a =
    match act with
    | Op1 r -> C "asd" |> r 
    | Op2 r -> C 5 |> r

let test<'a> (act: Op<'a>) =
    let x: Data<_> = operate act
    printfn "%A" x

test (Op2 id)