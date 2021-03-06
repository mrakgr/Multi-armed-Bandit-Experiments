-- Continued from monads_v4.hs

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where

import Control.Monad.Writer hiding ((>>))
import qualified Prelude as P
import Data.String (fromString)
import qualified Data.ByteString.Char8 as B
import Data.ByteString.Char8 (ByteString)
import Prelude hiding ((+),(-),(*),(/),(<=),(<),(==),(>),(>=),negate,exp,log,tanh)

infixl 1 |>
(|>) = flip ($)

newtype Method args = Method {method' :: (ByteString, args)}

class CudaOuter repr where
  include :: ByteString -> repr ()
  externCBlock :: repr () -> repr ()
  method :: ByteString -> repr (ins -> outs) -> repr (Method (ins -> outs))

data Statements =
  Statement ByteString
  | Indent
  | Dedent
  deriving Show

quote x = ["\"",x,"\""] |> B.concat

-- type StatementsParser = Writer [Statements] -- Ok
-- type StatementsParser = MonadWriter [Statements] m => m -- Error
newtype StatementsParser m x = StatementsParser {st :: m x}

instance MonadWriter [Statements] m => CudaOuter (StatementsParser m) where
  include x = [quote x |> Statement] |> tell |> StatementsParser
  -- externCBlock (StatementsParser body) =
  --   return ((),\body' -> [[Indent],body',[Dedent]] |> concat)
  --   |> pass |> StatementsParser

  -- method name body = -- These variables are simplified right now.
  --   let prefix = "__global__ void" in
  --   let args = "(int *x)" in
  --   let body_op = "{" in
  --   -- let (f,body') = runWriter body in
  --   let body_end = "}" in
  --   let pt1 = [[prefix, name, args, body_op] |> B.intercalate " " |> Statement] in
  --   let pt2 body' = [[Indent], body', [Dedent]] |> concat in
  --   let pt3 = [Statement body_end] in do
  --   f <- st body
  --   return (Method(name,f),\body' -> [pt1,pt2 body',pt3] |> concat) |> pass |> StatementsParser

--- The outer level of the Cuda compiler language.
cuda_kernel_module kernel_name method_body_macro = do
  include "thrust/tuple.h"
  include "thrust/functional.h"
  include "cub/cub.cuh"
  externCBlock $ do
    method kernel_name method_body_macro
    return ()

empty_function =
  let f () = () in
  writer (f, [Statement "return (); // Nothing here yet"])

-- test_outer =
--   cuda_kernel_module "TestKernel" empty_function
--   |> execWriter

main = print "Hello"
