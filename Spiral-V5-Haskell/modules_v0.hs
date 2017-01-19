{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Main where

import qualified Data.ByteString as B

infixl 0 |>
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

  If :: Show x => CudaExpr Bool -> CudaExpr x -> CudaExpr x -> CudaExpr x

eval :: CudaExpr x -> x
eval (C x) = x
eval (Add a b) = eval a + eval b
eval (Sub a b) = eval a - eval b
eval (Mul a b) = eval a * eval b
eval (Div a b) = eval a / eval b

eval (EQ' a b) = eval a == eval b
eval (LT' a b) = eval a < eval b
eval (LTE' a b) = eval a <= eval b
eval (GT' a b) = eval a > eval b
eval (GTE' a b) = eval a >= eval b
eval (If cond true false) = if eval cond then eval true else eval false

p = id
pw x = ["(", p x, ")"] |> concat

binop a x b = [pr a, p x, pr b] |> concat |> pw
triop x a y b z c = [p x, pr a, p y, pr b, p z, pr c] |> concat |> pw

pr :: Show x => CudaExpr x -> String
pr (C x) = show x
pr (Add a b) = binop a "+" b
pr (Mul a b) = binop a "*" b
pr (Sub a b) = binop a "-" b
pr (Div a b) = binop a "/" b

pr (EQ' a b) = binop a "==" b
pr (LT' a b) = binop a "<" b
pr (LTE' a b) = binop a "<=" b
pr (GT' a b) = binop a ">" b
pr (GTE' a b) = binop a ">=" b
pr (If cond true false) = triop "if" cond "then" true "else" false

q = (Add (C (2 :: Float)) (C 3)) |> Mul (C 6) |> pr

main :: IO ()
main = print "Hello"
