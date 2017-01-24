{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Prelude as P
import qualified Data.ByteString.Char8 as B
import Data.ByteString.Char8 (ByteString)
import Prelude hiding ((+),(-),(*),(/),(<=),(<),(==),(>),(>=))

infixl 1 |>
(|>) = flip ($)

class CudaConst repr where
  c :: Show x => x -> repr x

class CudaArith repr where
  (~-) :: Num x => repr x -> repr x
  (+) :: Num x => repr x -> repr x -> repr x
  (-) :: Num x => repr x -> repr x -> repr x
  (*) :: Num x => repr x -> repr x -> repr x
  (/) :: Fractional x => repr x -> repr x -> repr x

class CudaComp repr where
  (<=) :: Ord x => repr x -> repr x -> repr Bool
  (<) :: Ord x => repr x -> repr x -> repr Bool
  (==) :: Eq x => repr x -> repr x -> repr Bool
  (>) :: Ord x => repr x -> repr x -> repr Bool
  (>=) :: Ord x => repr x -> repr x -> repr Bool

class CudaBoolOps repr where
  (||) :: repr Bool -> repr Bool -> repr Bool
  (&&) :: repr Bool -> repr Bool -> repr Bool

class CudaCond repr where
  if_ :: repr Bool -> repr x -> repr x -> repr x

newtype Eval x = Eval {eval :: x} deriving Show

instance CudaConst Eval where
  c = Eval

bo f a b = eval a `f` eval b |> Eval

instance CudaArith Eval where
  --(~-) = bo P.-
  (+) = bo (P.+)
  -- (-) = bo P.-
  -- (*) = bo P.*
  -- (/) = bo P./

-- instance CudaComp Eval where
--   (<=) = bo P.<=
--   (<) = bo P.<
--   (==) = bo P.==
--   (>) = bo P.>
--   (>=) = bo P.>=
--
-- instance CudaBoolOps Eval where
--   (&&) = bo P.&&
--   (||) = bo P.||
--
-- instance CudaCond Eval where
--   if_ cond true false =
--     Eval $ if eval cond then eval true else eval false
--
-- newtype Print x = Print {pr :: ByteString} deriving Show
--
-- parenths x = ["(", x, ")"] |> pw'
-- pw' = B.intercalate " "
-- pw = parenths . pw'
--
-- fun_call f x = [f, "(", args, ")"] |> pw' where
--   args' = map pr x
--   args = B.intercalate ", " args'
--
-- unop x a = [x, pr a] |> pw |> Print
-- binop x a b = [pr a, x, pr b] |> pw |> Print
-- triop x y z a b c = [x, pr a, y, pr b, z, pr c] |> pw |> Print
--
-- instance CudaConst Print where
--   c = Print . B.pack . show
--
-- instance CudaArith Print where
--   (+) = binop "+"
--   (-) = binop "-"
--   (*) = binop "*"
--   (/) = binop "/"
--
-- instance CudaComp Print where
--   (<=) = binop "<="
--   (<) = binop "<"
--   (==) = binop "=="
--   (>) = binop ">"
--   (>=) = binop ">="
--
-- instance CudaBoolOps Print where
--   (&&) = binop "&&"
--   (||) = binop "||"
--
-- instance CudaCond Print where
--   if_ = triop "if" "then" "else"
--
-- test = (c 1) + (c 2) * (c 3)
--
-- q = eval test
-- w = (pr :: Print Float -> ByteString) test
--
main = print "Hello"
