{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import qualified Data.ByteString.Char8 as B
import Data.ByteString (ByteString)

infixl 1 |>
(|>) = flip ($)

data CudaExpr x where
  C :: Show x => x -> CudaExpr x

  Add :: (Show x, Num x) => CudaExpr x -> CudaExpr x -> CudaExpr x
  Sub :: (Show x, Num x) => CudaExpr x -> CudaExpr x -> CudaExpr x
  Mul :: (Show x, Num x) => CudaExpr x -> CudaExpr x -> CudaExpr x
  Div :: (Show x, Fractional x) => CudaExpr x -> CudaExpr x -> CudaExpr x

  LT' :: (Show x, Ord x) => CudaExpr x -> CudaExpr x -> CudaExpr Bool
  LTE' :: (Show x, Ord x) => CudaExpr x -> CudaExpr x -> CudaExpr Bool
  EQ' :: (Show x, Eq x) => CudaExpr x -> CudaExpr x -> CudaExpr Bool
  GT' :: (Show x, Ord x) => CudaExpr x -> CudaExpr x -> CudaExpr Bool
  GTE' :: (Show x, Ord x) => CudaExpr x -> CudaExpr x -> CudaExpr Bool

  Exp :: (Show x, Num x) => CudaExpr x -> CudaExpr x
  Log :: (Show x, Num x) => CudaExpr x -> CudaExpr x
  Tanh :: (Show x, Num x) => CudaExpr x -> CudaExpr x
  Neg :: (Show x, Num x) => CudaExpr x -> CudaExpr x

  Or :: CudaExpr Bool -> CudaExpr Bool -> CudaExpr Bool
  And :: CudaExpr Bool -> CudaExpr Bool -> CudaExpr Bool

  If :: Show x => CudaExpr Bool -> CudaExpr x -> CudaExpr x -> CudaExpr x

parenths x = ["(", x, ")"] |> pw'
pw' = B.intercalate " "
pw = parenths . pw'

fun_call f x = [f, "(", args, ")"] |> pw' where
  args' = map pr x
  args = B.intercalate ", " args'
unop x a = [x, pr a] |> pw
binop a x b = [pr a, x, pr b] |> pw
triop x a y b z c = [x, pr a, y, pr b, z, pr c] |> pw

pr :: Show x => CudaExpr x -> ByteString
pr (C x) = B.pack $ show x
pr (Add a b) = binop a "+" b
pr (Mul a b) = binop a "*" b
pr (Sub a b) = binop a "-" b
pr (Div a b) = binop a "/" b

pr (EQ' a b) = binop a "==" b
pr (LT' a b) = binop a "<" b
pr (LTE' a b) = binop a "<=" b
pr (GT' a b) = binop a ">" b
pr (GTE' a b) = binop a ">=" b

pr (Or a b) = binop a "||" b
pr (And a b) = binop a "&&" b

pr (Neg x) = unop "-" x
pr (Exp x) = fun_call "exp" [x]
pr (Log x) = fun_call "log" [x]
pr (Tanh x) = fun_call "tanh" [x]

pr (If cond true false) = triop "if" cond "then" true "else" false

--q = (Add (C (2 :: Float)) (C 3)) |> Mul (C 6) |> Log |> pr

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

type family ResType t :: * where
  ResType (x, y) = ((x, x), (y, y))
  ResType (x, y, z) = ((x, x), (y, y), (z, z))

map_into_prim_adj :: CudaVariable x -> CudaVariable (ResType x)
map_into_prim_adj x =
  let f = varar1d_into_prim_adj in
  case x of
    VarTuple2 a b -> VarTuple2 (f a) (f b)
    VarTuple3 a b c -> VarTuple3 (f a) (f b) (f c)

q = map_into_prim_adj x1

main :: IO ()
main = print "Hello"
