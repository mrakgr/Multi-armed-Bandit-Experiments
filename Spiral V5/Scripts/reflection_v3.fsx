﻿#load "load-references-release.fsx"

open System

let man = Reflection.Assembly.Load("ManagedCuda, Version=7.5.7.0, Culture=neutral, PublicKeyToken=242d898828717aa0")
let t =
    man.GetTypes()
    |> Array.find (fun x ->
        x.Name.Contains "CudaDeviceVariable"
        )
t.FullName