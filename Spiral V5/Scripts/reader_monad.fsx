// I bashed them in my 2016 review, but they are what I need here.

type ReaderBuilder() =
    member inline t.Return a = fun r -> a
    member inline t.Bind(a,f) = fun r -> f (a r) r
    member inline t.ReturnFrom x = x

let reader = ReaderBuilder()

let asd r = r+5

let test asd = reader {
    let! a1 = asd
    let! a2 = asd
    let! a3 = asd
    return a1+a2+a3
    }

test asd 0
