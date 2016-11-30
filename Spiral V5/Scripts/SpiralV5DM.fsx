#load "SpiralV5CodeGen.fsx"

open SpiralV5

open ManagedCuda
open ManagedCuda.BasicTypes
open ManagedCuda.VectorTypes
open ManagedCuda.CudaBlas
open ManagedCuda.CudaRand
open ManagedCuda.NVRTC
open ManagedCuda.CudaDNN

open System
open System.Diagnostics
open System.IO
open System.Collections.Generic
open System.Runtime.InteropServices

type SpiralInitializer2D =
| Init2DV1 of float32[,]
| Init2DV2 of float32[][]

type SpiralInitializer3D =
| Init2DV1 of float32[,,]
| Init2DV2 of float32[][][]

type SpiralInitializer =
| InitNo of dims: int list
| InitRandom of dims: int list
| InitHost1D of float32[]
| InitHost2D of SpiralInitializer2D
| InitHost3D of SpiralInitializer3D

type SpiralExpr =
| BaseNode of id: int * SpiralInitializer