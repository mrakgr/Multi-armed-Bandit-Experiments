open System

let a = typeof<obj>
let b = typeof<obj>

a = b // true
Object.ReferenceEquals(a,b) // true

