{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Main where
infixl 1 |>
(|>) = flip ($)

class CudaConst repr where
  c :: Show x => x -> repr x

class CudaArith repr where
  add :: Num x => repr x -> repr x -> repr x
  sub :: Num x => repr x -> repr x -> repr x
  mul :: Num x => repr x -> repr x -> repr x
  div :: Fractional x => repr x -> repr x -> repr x

newtype Eval x = Eval {eval :: x} deriving Show

instance CudaConst Eval where
  c = Eval

instance CudaArith Eval where
  add a b = eval a + eval b |> Eval
  sub a b = eval a - eval b |> Eval
  mul a b = eval a * eval b |> Eval
  div a b = eval a / eval b |> Eval

newtype Print x = Print {print' :: String} deriving Show

p = id
pw x = ["(", p x, ")"] |> concat

binop a x b =
  let pr = print' in
  [pr a, p x, pr b] |> concat |> pw

instance CudaConst Print where
  c = Print . show

instance CudaArith Print where
  add a b = binop a "+" b |> Print
  sub a b = binop a "-" b |> Print
  mul a b = binop a "*" b |> Print
  div a b = binop a "/" b |> Print

test = add (c 1) (c 2) |> mul (c 3)

q = eval test
w = (print' :: Print Float -> String) test

runTest a (f1, f2) = (f1 a, f2 a)

main = print "Hello"
