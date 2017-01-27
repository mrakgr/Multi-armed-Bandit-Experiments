{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}

module Main where

import Control.Monad.Writer hiding ((>>))
import qualified Prelude as P
import Data.String (fromString)
import qualified Data.ByteString.Char8 as B
import Data.ByteString.Char8 (ByteString)
import Prelude hiding ((+),(-),(*),(/),(<=),(<),(==),(>),(>=),negate,exp,log,tanh,(>>))

infixl 1 |>, >>
(|>) = flip ($)

class CudaOuter repr where
  include :: ByteString -> repr ()
  externCBlock :: [repr ()] -> repr ()
  method :: ByteString -> repr (ins -> outs) -> repr ()
  (>>) :: repr () -> repr () -> repr ()

data Statements =
  Statement ByteString
  | Indent
  | Dedent
  | Statements [Statements]
  deriving Show

newtype StatementsParser x = StatementsParser {st :: Statements}

quote x = ["\"",x,"\""] |> B.concat

instance CudaOuter StatementsParser where
  include x = quote x |> Statement |> StatementsParser
  externCBlock body =
    let body' = body |> map st in
    [Indent, Statements body', Dedent] |> Statements |> StatementsParser

  method name body = -- These variables are simplified right now.
    let prefix = "__global__ void" in
    let args = "(int *x)" in
    let body_op = "{" in
    let body' = st body in
    let body_end = "}" in
    let pt1 = [[prefix, name, args, body_op] |> B.intercalate " " |> Statement] in
    let pt2 = [Indent, body', Dedent] in
    let pt3 = [Statement body_end] in
    [pt1,pt2,pt3] |> concat |> Statements |> StatementsParser

  (>>) a b = [st a, st b] |> Statements |> StatementsParser

instance Monad StatementsParser where
  (>>=) a f =
    let a' = st a in
    let b = f a' |> st in
    [a',b] |> Statements |> StatementsParser

--- The outer level of the Cuda compiler language.
cuda_kernel_module kernel_name method_body_macro =
  include "thrust/tuple.h" >>
  include "thrust/functional.h" >>
  include "cub/cub.cuh" >>
  externCBlock
    [method kernel_name method_body_macro]

empty_statement =
  Statement "return (); // Nothing here yet" |> StatementsParser

test_outer =
  cuda_kernel_module "TestKernel" empty_statement
  |> st

main = print "Hello"
