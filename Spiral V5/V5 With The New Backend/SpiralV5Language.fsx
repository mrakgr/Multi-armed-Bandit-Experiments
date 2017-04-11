#load "SpiralV5Ops.fsx"

open SpiralV5Ops
open SpiralV5DevVar

type SpiralExprType =
| TensorT of uint64 [] * SpiralDeviceVarType

type SpiralExpr =
| S of string
| SS of SpiralExpr list
