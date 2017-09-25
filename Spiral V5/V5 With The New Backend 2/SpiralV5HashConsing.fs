module Spiral.HashConsing

open System
open System.Runtime.InteropServices

[<CustomComparison;CustomEquality>]
type ConsedNode<'a> =
    {
    node: 'a
    tag: int
    hkey: int
    }

    override x.ToString() = sprintf "%A" x.node
    override x.GetHashCode() = x.hkey
    override x.Equals(y) = 
        match y with 
        | :? ConsedNode<'a> as y -> x.tag = y.tag
        | _ -> failwith "Invalid equality for HashConsed."

    interface IComparable with
        member x.CompareTo(y) = 
            match y with
            | :? ConsedNode<'a> as y -> compare x.tag y.tag
            | _ -> failwith "Invalid comparison for HashConsed."

let consednode_gentag =
    let mutable i = 0
    fun () -> i <- i+1; i

type HashConsTable<'a> =
    {
    table: ResizeArray<GCHandle> []
    }

let hashcons_table (x:HashConsTable<_>) = x.table
let hashcons_create i = {table = Array.init i (fun _ -> ResizeArray(0))}

let hashcons_add (t: HashConsTable<'a>) (x: 'a) =
    let hkey = hash x &&& Int32.MaxValue
    let table = hashcons_table t
    let bucket = table.[hkey % (Array.length table)]
    let sz = bucket.Count

    let rec loop empty_pos i =
        if i < sz then
            match bucket.[i].Target with
            | null -> loop i (i+1)
            | :? ConsedNode<'a> as y when x = y.node -> y
            | _ -> loop empty_pos (i+1)
        else
            let node = {node=x; hkey=hkey; tag=consednode_gentag()}
            if empty_pos <> -1 then
                let mutable m = bucket.[empty_pos]
                m.Target <- node
            else
                bucket.Add (GCHandle.Alloc(node,GCHandleType.Weak))
            node

    loop -1 0 // `-1` indicates the state of no empty bucket
