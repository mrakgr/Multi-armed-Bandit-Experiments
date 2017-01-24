{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}

module Main where

import qualified Prelude as P
import Data.String (fromString)
import qualified Data.ByteString.Char8 as B
import Data.ByteString.Char8 (ByteString)
import Prelude hiding ((+),(-),(*),(/),(<=),(<),(==),(>),(>=),negate,exp,log,tanh)

-- ifThenElse True true false = true
-- ifThenElse False true false = false

infixl 1 |>
(|>) = flip ($)

class CudaConst repr where
  c :: Show x => x -> repr x

infixl 6 +,-
infixl 7 *,/
class CudaArith repr where
  negate :: Num x => repr x -> repr x
  (+) :: Num x => repr x -> repr x -> repr x
  (-) :: Num x => repr x -> repr x -> repr x
  (*) :: Num x => repr x -> repr x -> repr x
  (/) :: Fractional x => repr x -> repr x -> repr x

infix 4 <=,<,==,>,>=
class CudaComp repr where
  (<=) :: Ord x => repr x -> repr x -> repr Bool
  (<) :: Ord x => repr x -> repr x -> repr Bool
  (==) :: Eq x => repr x -> repr x -> repr Bool
  (>) :: Ord x => repr x -> repr x -> repr Bool
  (>=) :: Ord x => repr x -> repr x -> repr Bool

class CudaBoolOps repr where
  infixr 2 ||
  (||) :: repr Bool -> repr Bool -> repr Bool
  infixr 3 &&
  (&&) :: repr Bool -> repr Bool -> repr Bool

class CudaCond repr where
  ifThenElse :: repr Bool -> repr x -> repr x -> repr x

class CudaFun repr where
  exp :: Floating x => repr x -> repr x
  log :: Floating x => repr x -> repr x
  tanh :: Floating x => repr x -> repr x

newtype Eval x = Eval {eval :: x} deriving Show

instance CudaConst Eval where
  c = Eval

bo f a b = eval a `f` eval b |> Eval

instance CudaArith Eval where
  negate = Eval . P.negate . eval
  (+) = bo (P.+)
  (-) = bo (P.-)
  (*) = bo (P.*)
  (/) = bo (P./)

instance CudaComp Eval where
  (<=) = bo (P.<=)
  (<) = bo (P.<)
  (==) = bo (P.==)
  (>) = bo (P.>)
  (>=) = bo (P.>=)

instance CudaBoolOps Eval where
  (&&) = bo (P.&&)
  (||) = bo (P.||)

instance CudaCond Eval where
  ifThenElse cond true false =
    case eval cond of
      True -> Eval $ eval true
      False -> Eval $ eval false

uf f = Eval . f . eval

instance CudaFun Eval where
  exp = uf P.exp
  log = uf P.log
  tanh = uf P.tanh

newtype Print x = Print {pr :: ByteString} deriving Show

parenths x = ["(", x, ")"] |> pw'
pw' = B.concat
pw = parenths . pw'

fun_call f x = [f, "(", args, ")"] |> pw' |> Print where
  -- args' = map pr x
  -- args = B.intercalate ", " args'
  args = pr x

space_out x = B.concat [" ", x, " "]
unop x a = [x, pr a] |> pw |> Print
binop x a b = [pr a, space_out x, pr b] |> pw |> Print
triop x y z a b c = [x, pr a, space_out y, pr b, space_out z, pr c] |> pw |> Print

instance CudaConst Print where
  c = Print . B.pack . show

instance CudaArith Print where
  negate = unop "-"
  (+) = binop "+"
  (-) = binop "-"
  (*) = binop "*"
  (/) = binop "/"

instance CudaComp Print where
  (<=) = binop "<="
  (<) = binop "<"
  (==) = binop "=="
  (>) = binop ">"
  (>=) = binop ">="

instance CudaBoolOps Print where
  (&&) = binop "&&"
  (||) = binop "||"

instance CudaCond Print where
  ifThenElse = triop "" "?" ":"

instance CudaFun Print where
  exp = fun_call "exp"
  log = fun_call "log"
  tanh = fun_call "tanh"

test = if c True then c 1 + c 2 * (- (c 3)) else c 999 |> log

q = eval test
w = pr test

main = print "Hello"
