// Hash tables for hash consing. Taken from: https://github.com/backtracking/ocaml-hashcons
// The OCaml version is by Jean-Christophe Filliatre
// The F# port is by Marko Grdinić
// License: LGPL 2.1

module Spiral.HashConsing

open System
open System.Runtime.InteropServices

let raise_not_found _ = raise <| System.Collections.Generic.KeyNotFoundException()
let error_on_collision x = failwith "There should never be a collision in this function."

type ConsedNode<'a>(node:'a, tag:int, hkey: int) =
    member val Tag = tag
    member val Node = node
    member val HKey = hkey

    override x.ToString() = sprintf "%A" node
    override x.GetHashCode() = hkey
    override x.Equals(y) = 
        match y with 
        | :? ConsedNode<'a> as y -> tag = y.Tag
        | _ -> failwith "Invalid equality for HashConsed."

    interface IComparable with
        member x.CompareTo(y) = 
            match y with
            | :? ConsedNode<'a> as y -> compare tag y.Tag
            | _ -> failwith "Invalid comparison for HashConsed."

type TableStats =
    {
    table_length: int
    number_of_entries: int
    sum_of_bucket_lengths: int
    smallest_bucket_length: int
    median_bucket_length: int
    biggest_bucket_length: int
    }

let gentag =
    let mutable i = 0
    fun () -> i <- i+1; i

type WeakArray<'a>(ar) = // Partly based on: https://www.codeproject.com/Articles/43042/WeakReferences-GCHandles-and-WeakArrays
    new i = WeakArray(Array.init i (fun _ -> GCHandle.Alloc(null,GCHandleType.Weak)))
    member val GCHandleArray = ar
    member __.Length = ar.Length
    member __.Item 
        with set i (v: 'a) = ar.[i].Target <- v
        and get(i) = 
            match ar.[i].Target with
            | null -> None
            | v -> Some (v :?> 'a)
        
    override __.Finalize() = 
        let rec loop i =
            if i < ar.Length then
                let x = ar.[i]
                if x.IsAllocated then x.Free(); loop (i+1)
        loop 0

    member x.Check i = match x.[i] with None -> false | Some _ -> true
    static member Blit (a: WeakArray<'a>) ao (b: WeakArray<'a>) bo sz = Array.blit a.GCHandleArray ao b.GCHandleArray bo sz

type ConsedTable<'a when 'a: equality>(table: WeakArray<ConsedNode<'a>> [], totsize: int, limit: int) =
    static let max_array_length = Int32.MaxValue
    let next_sz n = min (3*n/2+3) max_array_length

    new sz =
        let sz = max 7 sz in // There is no need to check for the upper bound since it is System.Int32.MaxValue for .NET.
        let empty_bucket = WeakArray 0 in // This sharing is not a mistake. Empty buckets get replaced in the `Add` method.
        ConsedTable(Array.create sz empty_bucket,0,3) 

    member val Table = table with get,set
    member val TotalSize = totsize with get,set // sum of the bucket sizes
    member val Limit = limit with get,set // max ratio totsize/table length

    member t.Clear =
        let empty_bucket = WeakArray 0
        for i=0 to Array.length t.Table - 1 do t.Table.[i] <- empty_bucket
        t.TotalSize <- 0
        t.Limit <- 3

    member t.FoldBack f init =
        let rec fold_bucket i (b: WeakArray<_>) accu =
            if i >= b.Length then accu else
                match b.[i] with
                | None -> fold_bucket (i+1) b accu
                | Some v -> fold_bucket (i+1) b (f v accu)
        Array.foldBack (fold_bucket 0) t.Table init
        
    member t.Iter f =
        let rec iter_bucket i (b: WeakArray<_>) =
            if i >= b.Length then () else
                match b.[i] with
                | None -> iter_bucket (i+1) b
                | Some v -> f v; iter_bucket (i+1) b
        Array.iter (iter_bucket 0) t.Table

    member t.Count =
        let rec count_bucket i (b: WeakArray<_>) accu =
            if i >= b.Length then accu else
                if b.Check i then accu+1 else accu
                |> count_bucket (i+1) b
        Array.foldBack (count_bucket 0) t.Table 0

    member t.Resize =
        let oldlen = Array.length t.Table
        let newlen = next_sz oldlen
        if newlen > oldlen then
            let newt = ConsedTable newlen
            newt.Limit <- t.Limit + 100 // prevent resizing of newt
            t.Iter newt.Add
            t.Table <- newt.Table
            t.Limit <- t.Limit+2

    member t.Add d =
        let index = d.HKey % (Array.length t.Table)
        let bucket = t.Table.[index]
        let sz = bucket.Length
        let rec loop i =
            if i >= sz then
                let newsz = min (sz + 3) max_array_length
                if newsz <= sz then failwith "hash bucket cannot grow more"
                let newbucket = WeakArray newsz
                WeakArray.Blit bucket 0 newbucket 0 sz
                newbucket.[i] <- d
                t.Table.[index] <- newbucket
                t.TotalSize <- t.TotalSize + (newsz - sz)
                if t.TotalSize > t.Limit * Array.length t.Table then t.Resize
            else
                if bucket.Check i then loop (i+1)
                else bucket.[i] <- d
        loop 0

    member t.HashCons d =
        let hkey = hash d &&& Int32.MaxValue // Zeroes out the sign bit. Is unnecessary in the Ocaml version.
        let index = hkey % (Array.length t.Table)
        let bucket = t.Table.[index]
        let sz = bucket.Length
        let rec loop i =
            if i >= sz then
                let hnode = ConsedNode(d,gentag(),hkey)
                t.Add hnode
                hnode
            else
                match bucket.[i] with
                | Some v when v.Node = d -> v
                | _ -> loop (i+1)
        loop 0

    member t.Stats =
        let len = Array.length t.Table
        let lens = t.Table |> Array.map (fun x -> x.Length)
        Array.sortInPlace lens
        let totlen = Array.sum lens
        { 
        table_length = len; number_of_entries = t.Count; sum_of_bucket_lengths = totlen
        smallest_bucket_length = lens.[0]; median_bucket_length = lens.[len/2]; biggest_bucket_length = Array.last lens 
        }
