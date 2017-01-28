-- Continued from monads_v4.hs

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

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

--- The outer level of the Cuda compiler language.
cuda_kernel_module kernel_name method_body_macro = do
  include "thrust/tuple.h"
  include "thrust/functional.h"
  include "cub/cub.cuh"
  externCBlock $ do
    method kernel_name method_body_macro
    return ()

main = print "Hello"
