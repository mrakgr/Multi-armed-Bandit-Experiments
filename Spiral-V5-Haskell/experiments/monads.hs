-- I have no idea what is going on here.

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
-- {-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Control.Monad.Writer
import Control.Monad.State

-- stati :: Int -> State Int Int
stati x = state $ const x
logi (x :: [Char]) = lift $ writer (2,x)

-- test_monad :: WriterT d m x
test_monad = do
  logi "Hello, "
  logi "Writter."

-- w = do
--   x <- runState $ runWriterT test_monad
--   return x

main = do
  -- (q,result) <- w
  print "hello"
