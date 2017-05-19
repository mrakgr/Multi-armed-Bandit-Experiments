type TypecheckerResult<'a,'b> =
| Succ of 'a
| Fail of 'b

let a f on_fail on_succ =
    f   (fun x -> 
            let t = "Not good" :: x
            on_fail t)
        (fun x ->
            let t = x+1
            on_succ t)

let f on_fail on_succ = 1
    //|> on_succ

let q: TypecheckerResult<float,string> = a f Fail (float >> Succ)