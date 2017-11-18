open System
open System.Collections.Generic

type Q =
    | A of string
    | B of int

let a = A ""
a.GetHashCode()

let b = A ""
LanguagePrimitives.PhysicalHash b
b.GetHashCode()

let d = Dictionary(HashIdentity.Reference)
d.Add(a,1)
d.Add(b,2)
