open System

let rec f _ = 
    (x: Threading.Timer).Dispose()
    failwith "Bam."
and x = new Threading.Timer(f,null,500,0)

//x.Dispose()