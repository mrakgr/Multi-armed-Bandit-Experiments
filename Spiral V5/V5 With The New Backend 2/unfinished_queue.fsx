// I started this because I realized I cannot pass Spiral's inbuilt tuples as generic types to the .NET side.
// This queue uses a tuple of queues representation and serves as an example of how this might be done with 
// arrays, in order to go from array of structs to struct of array representantion.

// Unfortuantely, the queue can't take Spiral's inbuilt union types either, so it is not particularly useful.
// I will have to either build all the essential datatypes directly into the language, or preferably implement them in it
// so that I can apply the full power of partial evaluation to them.

// This module is a of yet, untested.
inl queue = mscorlib ."System.Collections.Generic.Queue"

inl rec create = function
    | _ :: _ as x -> Tuple.map create x
    | x -> 
        inl q = queue x // Type application
        q.Enqueue x // Adds the first element
        q

inl enqueue q x =
    inl rec loop = function
        | x :: xs, x' :: xs' -> loop (x,x'); loop (xs,xs')
        | x,x' -> x.Enqueue x'
    loop (Tuple.zip (q,x))

inl rec dequeue = function
    | _ :: _ as x -> Tuple.map dequeue x
    | x -> x.Dequeue()
