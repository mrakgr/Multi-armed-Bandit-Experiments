open System
open System.Reflection

let m = Assembly.Load "ManagedCuda, Version=7.5.7.0, Culture=neutral, PublicKeyToken=242d898828717aa0"

type Q = {
    mutable b : int
    }

let x = {b = 2}
x.b <- 3