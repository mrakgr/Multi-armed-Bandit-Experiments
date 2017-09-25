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

type WeakResizeArray<'a>(ar) = // Partly based on: https://www.codeproject.com/Articles/43042/WeakReferences-GCHandles-and-WeakArrays
    new i = new WeakResizeArray<_>(Array.init i (fun _ -> GCHandle.Alloc(null,GCHandleType.Weak)) |> ResizeArray)
    member val GCHandleArray = ar
    member __.Count = ar.Count
    member __.Item 
        with set i (v: 'a) = 
            let mutable x = ar.[i]
            x.Target <- v
        and get(i) = 
            match ar.[i].Target with
            | null -> None
            | v -> Some (v :?> 'a)
        
    member x.Check i = match x.[i] with None -> false | Some _ -> true
    member x.Add v = ar.Add(GCHandle.Alloc(v,GCHandleType.Weak))

    interface IDisposable with
        member __.Dispose()= 
            let rec loop i =
                if i < ar.Count then
                    let x = ar.[i]
                    if x.IsAllocated then x.Free(); loop (i+1)
            loop 0

type ConsedTable<'a when 'a: equality>(table: WeakResizeArray<ConsedNode<'a>> [], totsize: int, limit: int) =
    static let max_array_length = Int32.MaxValue
    let next_sz n = min (3*n/2+3) max_array_length

    new sz =
        let sz = max 7 sz in
        new ConsedTable<_>(Array.create sz (new WeakResizeArray<_>(0)),0,3)

    member val Table = table with get,set
    member val TotalSize = totsize with get,set // sum of the bucket sizes
    member val Limit = limit with get,set // max ratio totsize/table length

    member t.Clear =
        for i=0 to Array.length t.Table - 1 do t.Table.[i] <- new WeakResizeArray<_>(0)
        t.TotalSize <- 0
        t.Limit <- 3

    member t.FoldBack f init =
        let rec fold_bucket i (b: WeakResizeArray<_>) accu =
            if i >= b.Count then accu else
                match b.[i] with
                | None -> fold_bucket (i+1) b accu
                | Some v -> fold_bucket (i+1) b (f v accu)
        Array.foldBack (fold_bucket 0) t.Table init
        
    member t.Iter f =
        let rec iter_bucket i (b: WeakResizeArray<_>) =
            if i >= b.Count then () else
                match b.[i] with
                | None -> iter_bucket (i+1) b
                | Some v -> f v; iter_bucket (i+1) b
        Array.iter (iter_bucket 0) t.Table

    member t.Count =
        let rec count_bucket i (b: WeakResizeArray<_>) accu =
            if i >= b.Count then accu else
                if b.Check i then accu+1 else accu
                |> count_bucket (i+1) b
        Array.foldBack (count_bucket 0) t.Table 0

    member t.Resize =
        let oldlen = Array.length t.Table
        let newlen = next_sz oldlen
        if newlen > oldlen then
            let newt = new ConsedTable<_>(newlen)
            newt.Limit <- t.Limit + 100 // prevent resizing of newt
            t.Iter newt.Add
            t.Table <- newt.Table
            t.Limit <- t.Limit+2

    member t.Add d =
        let index = d.HKey % (Array.length t.Table)
        let bucket = t.Table.[index]
        let sz = bucket.Count
        let rec loop i =
            if i >= sz then
                bucket.Add d
                t.TotalSize <- t.TotalSize + 1
                if t.TotalSize > t.Limit * Array.length t.Table then t.Resize
            else
                if bucket.Check i then loop (i+1)
                else bucket.[i] <- d
        loop 0

    member t.HashCons d =
        let hkey = hash d &&& Int32.MaxValue // Zeroes out the sign bit. Is unnecessary in the Ocaml version.
        let index = hkey % (Array.length t.Table)
        let bucket = t.Table.[index]
        let sz = bucket.Count
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
        let lens = t.Table |> Array.map (fun x -> x.Count)
        Array.sortInPlace lens
        let totlen = Array.sum lens
        { 
        table_length = len; number_of_entries = t.Count; sum_of_bucket_lengths = totlen
        smallest_bucket_length = lens.[0]; median_bucket_length = lens.[len/2]; biggest_bucket_length = Array.last lens 
        }

    interface IDisposable with
        member __.Dispose() = table |> Seq.iter (fun x -> (x :> IDisposable).Dispose())