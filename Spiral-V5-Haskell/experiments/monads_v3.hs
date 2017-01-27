-- This thing is such a mess. Let me try again.

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}

module Main where

import Control.Monad.Writer
import qualified Prelude as P
import Data.String (fromString)
import qualified Data.ByteString.Char8 as B
import Data.ByteString.Char8 (ByteString)
import Prelude hiding ((+),(-),(*),(/),(<=),(<),(==),(>),(>=),negate,exp,log,tanh)

infixl 1 |>
(|>) = flip ($)

class CudaOuter repr where
  include :: ByteString -> Writer (repr ()) ()
  externCBlock :: [repr ()] -> Writer (repr ()) ()
  method :: ByteString -> repr (ins -> outs) -> Writer (repr ()) ()

data Statements =
  Statement ByteString
  | Indent
  | Dedent
  | Statements [Statements]
  deriving Show

newtype StatementsParser x = StatementsParser {st :: Writer [Statements] ()}

quote x = ["\"",x,"\""] |> B.concat

instance CudaOuter StatementsParser where
  include x = tell $ StatementsParser $ writer ((), [Statement $ quote x])
  externCBlock body =
    -- let f x = writer ((),x) in
    let body' = body |> map (execWriter . st) |> concat in
    [Indent, Statements body', Dedent] |> tell |> StatementsParser |> tell

  method name body = -- These variables are simplified right now.
    -- let f x = writer ((),x) in
    let prefix = "__global__ void" in
    let args = "(int *x)" in
    let body_op = "{" in
    let body' = st body |> execWriter in
    let body_end = "}" in
    let pt1 = [[prefix, name, args, body_op] |> B.intercalate " " |> Statement] in
    let pt2 = [Indent, Statements body', Dedent] in
    let pt3 = [Statement body_end] in
    [pt1,pt2,pt3] |> concat |> tell |> StatementsParser |> tell

--- The outer level of the Cuda compiler language.
-- cuda_kernel_module
--   :: CudaOuter repr => ByteString -> repr (ins -> outs) -> Writer [repr ()] ()
cuda_kernel_module kernel_name method_body_macro = execWriter $ do
  x <- include "thrust/tuple.h"
  return ()
  -- include "thrust/functional.h"
  -- include "cub/cub.cuh"
  -- externCBlock [method kernel_name method_body_macro]
  -- return ()


empty_statement =
  StatementsParser $ writer ((),[Statement "return (); // Nothing here yet"])

-- test_outer =
--   cuda_kernel_module "TestKernel" empty_statement
--   |> map (execWriter . st)
--   |> concat

main = print "Hello"
