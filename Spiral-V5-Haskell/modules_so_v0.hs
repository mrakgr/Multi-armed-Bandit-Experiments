{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Char8 as B
import Data.ByteString (ByteString)

infixl 1 |>
(|>) = flip ($)

data CudaVarAr2d x where VarAr2d :: CudaVarScalar Int -> CudaVarScalar Int -> ByteString -> CudaVarAr2d x
data CudaVarAr1d x where VarAr1d :: CudaVarScalar Int -> ByteString -> CudaVarAr1d x
data CudaVarScalar x where VarScalar :: ByteString -> CudaVarScalar x

data CudaVariable x where
  VarAr2d' :: CudaVarAr2d x -> CudaVariable x
  VarAr1d' :: CudaVarAr1d x -> CudaVariable x
  VarScalar' :: CudaVarScalar x -> CudaVariable x
  VarTuple2 :: CudaVariable x -> CudaVariable y -> CudaVariable (x,y)
  VarTuple3 :: CudaVariable x -> CudaVariable y -> CudaVariable z -> CudaVariable (x,y,z)

size = VarScalar "size"
x1 = VarAr1d' $ VarAr1d size "x1"
x2 = VarAr1d' $ VarAr1d size "x2"
inp = VarTuple2 x1 x2

o1 = VarAr1d' $ VarAr1d size "o1"
o2 = VarAr1d' $ VarAr1d size "o2"
outp = VarTuple2 o1 o2

-- Later I intend to cover all the cases.
varar1d_into_prim_adj :: CudaVariable x -> CudaVariable (x,x)
varar1d_into_prim_adj (VarAr1d' (VarAr1d size name)) = VarTuple2 x1 x2 where
  f suffix = VarAr1d' (VarAr1d size ([name,suffix] |> B.concat))
  x1 = f "_primal"
  x2 = f "_adjoint"

--map_into_prim_adj :: CudaVariable x -> CudaVariable x
map_into_prim_adj x =
  let f = varar1d_into_prim_adj in
  case x of
    VarTuple2 a b -> VarTuple2 (f a) (f b)
    VarTuple3 a b c -> VarTuple3 (f a) (f b) (f c)

main :: IO ()
main = print "Hello"
