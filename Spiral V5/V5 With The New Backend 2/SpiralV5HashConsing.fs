// Hash tables for hash consing. Taken from: https://github.com/backtracking/ocaml-hashcons
// The OCaml version is by Jean-Christophe Filliatre
// The F# port is by Marko Grdinić
// License: LGPL 2.1

module Spiral.HashConsing

open System
open System.Runtime.InteropServices

let raise_not_found _ = raise <| System.Collections.Generic.KeyNotFoundException()

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

type ConsedMap<'a,'b> =
    | Empty
    | Leaf of ConsedNode<'a> * 'b
    | Branch of int * int * ConsedMap<'a,'b> * ConsedMap<'a,'b>

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

module Map =
    (*  Maps of integers implemented as Patricia trees, following Chris
        Okasaki and Andrew Gill's paper `Fast Mergeable Integer Maps`.
        Patricia trees provide faster operations than standard library's
        module [Map], and especially very fast [union], [subset], [inter]
        and [diff] operations. *)

    (*  The idea behind Patricia trees is to build a trie on the
        binary digits of the elements, and to compact the representation
        by branching only one the relevant bits (i.e. the ones for which
        there is at least on element in each subtree). We implement here
        little-endian Patricia trees: bits are processed from
        least-significant to most-significant. The trie is implemented by
        the following type [t]. [Empty] stands for the empty trie, and
        [Leaf k] for the singleton [k]. (Note that [k] is the actual
        element.) [Branch (m,p,l,r)] represents a branching, where [p] is
        the prefix (from the root of the trie) and [m] is the branching
        bit (a power of 2). [l] and [r] contain the subsets for which the
        branching bit is respectively 0 and 1. Invariant: the trees [l]
        and [r] are not empty. *)

    (*  Example: the representation of the set {1,4,5} is
        {Branch(0,1,Leaf 4,Branch(1,4,Leaf 1,Leaf 5))}
        The first branching bit is the bit 0 (and the corresponding prefix
        is [0b0], not of use here), with {4} on the left and {1,5} on the
        right. Then the right subtree branches on bit 2 (and so has a branching
        value of 2^2 = 4), with prefix [0b01 = 1]. *)

    (*  Empty set and singletons. *)

    let is_empty = function Empty -> true | _ -> false
    let singleton k = Leaf k

    (*  Testing the occurrence of a value is similar to the search in a
        binary search tree, where the branching bit is used to select the
        appropriate subtree. *)

    let zero_bit k m = (k &&& m) = 0

    let rec mem (k: ConsedNode<'a>) (x: ConsedMap<'a,_>) = 
        match x with
        | Empty -> false
        | Leaf (j,_) -> k.Tag = j.Tag
        | Branch (_, m, l, r) -> mem k (if zero_bit k.Tag m then l else r)

    let rec find (k: ConsedNode<_>) (x: ConsedMap<'a,_>) = 
        match x with
        | Empty -> raise_not_found()
        | Leaf (j,v) -> if k.Tag = j.Tag then v else raise_not_found()
        | Branch (_, m, l, r) -> find k (if zero_bit k.Tag m then l else r)

    (*  The following operation [join] will be used in both insertion and
        union. Given two non-empty trees [t0] and [t1] with longest common
        prefixes [p0] and [p1] respectively, which are supposed to
        disagree, it creates the union of [t0] and [t1]. For this, it
        computes the first bit [m] where [p0] and [p1] disagree and create
        a branching node on that bit. Depending on the value of that bit
        in [p0], [t0] will be the left subtree and [t1] the right one, or
        the converse. Computing the first branching bit of [p0] and [p1]
        uses a nice property of twos-complement representation of integers. *)

    let lowest_bit x = x &&& (-x)
    let branching_bit p0 p1 = lowest_bit (p0 ^^^ p1)
    let mask p m = p &&& (m-1)

    (*  When comparing branching bits, one has to be careful with the sign bit *)
    let unsigned_lt n m = n >= 0 && (m < 0 || n < m)

    let join (p0,t0,p1,t1) =
        let m = branching_bit p0 p1
        if zero_bit p0 m then Branch (mask p0 m, m, t0, t1)
        else Branch (mask p0 m, m, t1, t0)

    (*  Then the insertion of value [k] in set [t] is easily implemented
        using [join].  Insertion in a singleton is just the identity or a
        call to [join], depending on the value of [k].  When inserting in
        a branching tree, we first check if the value to insert [k]
        matches the prefix [p]: if not, [join] will take care of creating
        the above branching; if so, we just insert [k] in the appropriate
        subtree, depending of the branching bit. *)

    let match_prefix k p m = (mask k m) = p

    let add c k x t =
        let rec ins = function
            | Empty -> Leaf (k, x)
            | Leaf (j,y) as t -> 
                if j.Tag = k.Tag then t 
                else join (k.Tag, Leaf (k, c (x, y)), j.Tag, t)
            | Branch (p,m,t0,t1) as t ->
                if match_prefix k.Tag p m then
                    if zero_bit k.Tag m then Branch (p, m, ins t0, t1)
                    else Branch (p, m, t0, ins t1)
                else join (k.Tag, Leaf (k, x), p, t)
        ins t

    (*  The code to remove an element is basically similar to the code of
        insertion. But since we have to maintain the invariant that both
        subtrees of a [Branch] node are non-empty, we use here the
        smart constructor [branch] instead of [Branch]. *)

    let branch = function
        | (_,_,Empty,t) -> t
        | (_,_,t,Empty) -> t
        | (p,m,t0,t1)   -> Branch (p,m,t0,t1)

    let remove (k: ConsedNode<_>) t =
        let rec rmv = function
            | Empty -> Empty
            | Leaf (j, _) as t -> if k.Tag = j.Tag then Empty else t
            | Branch (p,m,t0,t1) as t ->
                if match_prefix k.Tag p m then
                    if zero_bit k.Tag m then branch (p, m, rmv t0, t1)
                    else branch (p, m, t0, rmv t1)
                else
                    t
        rmv t

    (*  One nice property of Patricia trees is to support a fast union
        operation (and also fast subset, difference and intersection
        operations). When merging two branching trees we examine the
        following four cases: (1) the trees have exactly the same
        prefix; (2/3) one prefix contains the other one; and (4) the
        prefixes disagree. In cases (1), (2) and (3) the recursion is
        immediate; in case (4) the function [join] creates the appropriate
        branching. *)

    let swap (a,b) = b, a 

    let rec merge c x = 
        let inline f x = merge c x
        match x with
        | Empty, t  -> t
        | t, Empty  -> t
        | Leaf (k, x), t -> add c k x t
        | t, Leaf (k, x) -> add (c << swap) k x t
        | (Branch (p,m,s0,s1) as s), (Branch (q,n,t0,t1) as t) ->
            if m = n && match_prefix q p m then
                (* The trees have the same prefix. Merge the subtrees. *)
                Branch (p, m, f (s0,t0), f (s1,t1))
            elif unsigned_lt m n && match_prefix q p m then
                (* [q] contains [p]. Merge [t] with a subtree of [s]. *)
                if zero_bit q m then Branch (p, m, f (s0,t), s1)
                else Branch (p, m, s0, f (s1,t))
            elif unsigned_lt n m && match_prefix p q n then
                (* [p] contains [q]. Merge [s] with a subtree of [t]. *)
                if zero_bit p n then Branch (q, n, f (s,t0), t1)
                else Branch (q, n, t0, f (s,t1))
            else
                (* The prefixes disagree. *)
                join (p, s, q, t)

    let union c s t = merge c (s,t)

    (*  When checking if [s1] is a subset of [s2] only two of the above
        four cases are relevant: when the prefixes are the same and when the
        prefix of [s1] contains the one of [s2], and then the recursion is
        obvious. In the other two cases, the result is [false]. *)

    let rec subset s1 s2 = 
        match (s1,s2) with
        | Empty, _ -> true
        | _, Empty -> false
        | Leaf (k1, _), _ -> mem k1 s2
        | Branch _, Leaf _ -> false
        | Branch (p1,m1,l1,r1), Branch (p2,m2,l2,r2) ->
            if m1 = m2 && p1 = p2 then subset l1 l2 && subset r1 r2
            elif unsigned_lt m2 m1 && match_prefix p1 p2 m2 then
                if zero_bit p1 m2 then subset l1 l2 && subset r1 l2
                else subset l1 r2 && subset r1 r2
            else false


    (*  To compute the intersection and the difference of two sets, we
        still examine the same four cases as in [merge]. The recursion is
        then obvious. *)

    let rec inter c s1 s2 = 
        let inline f s1 s2 = inter c s1 s2
        match (s1,s2) with
        | Empty, _ -> Empty
        | _, Empty -> Empty
        | Leaf (k1,_), _ -> if mem k1 s2 then s1 else Empty
        | _, Leaf (k2,_) -> if mem k2 s1 then s2 else Empty
        | Branch (p1,m1,l1,r1), Branch (p2,m2,l2,r2) ->
            if m1 = m2 && p1 = p2 then merge c (f l1 l2, f r1 r2)
            elif unsigned_lt m1 m2 && match_prefix p2 p1 m1 then
                f (if zero_bit p2 m1 then l1 else r1) s2
            elif unsigned_lt m2 m1 && match_prefix p1 p2 m2 then
                f s1 (if zero_bit p1 m2 then l2 else r2)
            else Empty

    let rec diff c s1 s2 = 
        let inline f s1 s2 = diff c s1 s2
        match (s1,s2) with
        | Empty, _ -> Empty
        | _, Empty -> s1
        | Leaf (k1,_), _ -> if mem k1 s2 then Empty else s1
        | _, Leaf (k2,_) -> remove k2 s1
        | Branch (p1,m1,l1,r1), Branch (p2,m2,l2,r2) ->
            if m1 = m2 && p1 = p2 then merge c (f l1 l2, f r1 r2)
            elif unsigned_lt m1 m2 && match_prefix p2 p1 m1 then
                if zero_bit p2 m1 then merge c (f l1 s2, r1)
                else merge c (l1, f r1 s2)
            elif unsigned_lt m2 m1 && match_prefix p1 p2 m2 then
                if zero_bit p1 m2 then f s1 l2 else f s1 r2
            else s1

    (*  All the following operations ([cardinal], [iter], [fold], [for_all],
        [exists], [filter], [partition], [choose], [elements]) are
        implemented as for any other kind of binary trees. *)

    let rec cardinal = function
        | Empty -> 0
        | Leaf _ -> 1
        | Branch (_,_,t0,t1) -> cardinal t0 + cardinal t1

    let rec iter f = function
        | Empty -> ()
        | Leaf (k, v) -> f k v
        | Branch (_,_,t0,t1) -> iter f t0; iter f t1

    let rec fold f s accu = 
        match s with
        | Empty -> accu
        | Leaf (k,v) -> f k v accu
        | Branch (_,_,t0,t1) -> fold f t0 (fold f t1 accu)

    let rec for_all p = function
        | Empty -> true
        | Leaf (k,v) -> p k v
        | Branch (_,_,t0,t1) -> for_all p t0 && for_all p t1

    let rec exists p = function
        | Empty -> false
        | Leaf (k, v) -> p k v
        | Branch (_,_,t0,t1) -> exists p t0 || exists p t1

    let rec filter pr = function
        | Empty -> Empty
        | Leaf (k, v) as t -> if pr k v then t else Empty
        | Branch (p,m,t0,t1) -> branch (p, m, filter pr t0, filter pr t1)

    let error_on_collision x = failwith "There should never be a collision in this function."

    let partition p s =
        let rec part (t,f as acc) = function
            | Empty -> acc
            | Leaf (k, v) -> if p k then (add error_on_collision k v t, f) else (t, add error_on_collision k v f)
            | Branch (_,_,t0,t1) -> part (part acc t0) t1
        part (Empty, Empty) s

    let rec choose = function
        | Empty -> raise_not_found()
        | Leaf (k,v) -> k,v
        | Branch (_, _,t0,_) -> choose t0   (* we know that [t0] is non-empty *)

    let elements s =
        let rec elements_aux acc = function
            | Empty -> acc
            | Leaf (k,v) -> (k,v) :: acc
            | Branch (_,_,l,r) -> elements_aux (elements_aux acc l) r
        elements_aux [] s

    (*  There is no way to give an efficient implementation of [min_elt]
        and [max_elt], as with binary search trees.  The following
        implementation is a traversal of all elements, barely more
        efficient than [fold min t (choose t)] (resp. [fold max t (choose
        t)]). Note that we use the fact that there is no constructor
        [Empty] under [Branch] and therefore always a minimal
        (resp. maximal) element there. *)

    let rec min_elt = function
        | Empty -> raise_not_found()
        | Leaf (k,v) -> k,v
        | Branch (_,_,s,t) -> min (min_elt s) (min_elt t)

    let rec max_elt = function
        | Empty -> raise_not_found()
        | Leaf (k,v) -> k,v
        | Branch (_,_,s,t) -> max (max_elt s) (max_elt t)

    (*  Another nice property of Patricia trees is to be independent of the
        order of insertion. As a consequence, two Patricia trees have the
        same elements if and only if they are structurally equal. *)

    let equal = (=)
    let compare = compare

    let make l = List.foldBack (fun (k,x) -> add error_on_collision k x) l Empty

    let rec intersect s1 s2 = 
        match (s1,s2) with
        | Empty, _ -> false
        | _, Empty -> false
        | Leaf (k1,_), _ -> mem k1 s2
        | _, Leaf (k2,_) -> mem k2 s1
        | Branch (p1,m1,l1,r1), Branch (p2,m2,l2,r2) ->
            if m1 = m2 && p1 = p2 then intersect l1 l2 || intersect r1 r2
            elif unsigned_lt m1 m2 && match_prefix p2 p1 m1 then
                intersect (if zero_bit p2 m1 then l1 else r1) s2
            elif unsigned_lt m2 m1 && match_prefix p1 p2 m2 then
                intersect s1 (if zero_bit p1 m2 then l2 else r2)
            else
                false

