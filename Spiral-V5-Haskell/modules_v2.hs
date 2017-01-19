{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}

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

q = (Add (C (2 :: Float)) (C 3)) |> Mul (C 6) |> Log |> pr

main :: IO ()
main = print "Hello"
