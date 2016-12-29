type TotalSizeToken = TotalSizeToken with
    static member TotalSize(_: TotalSizeToken, (a,b,c,d,e)): int = a*b*c*d*e
    static member TotalSize(_: TotalSizeToken, (a,b,c,d)): int = a*b*c*d
    static member TotalSize(_: TotalSizeToken, (a,b,c)): int = a*b*c
    static member TotalSize(_: TotalSizeToken, (a,b)): int = a*b
    static member TotalSize(_: TotalSizeToken, x: int): int = x

let inline size_to_total_size x = 
    let call (t:^T) = ((^s or ^T) : (static member TotalSize: TotalSizeToken * ^s -> int) t, x)
    call TotalSizeToken

let t = size_to_total_size (1,5)