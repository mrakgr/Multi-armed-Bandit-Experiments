{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
-- {-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Control.Monad.Writer
import Control.Monad.State

add_state x = state $ \state -> (x + state, state)
log_something x = writer x

test_state = do
  q <- add_state 2
  w <- add_state 5
  e <- add_state 10
  return (q,w,e)

test_log = do
  log_something (0,["Hello, "])
  log_something (5,["World."])
  return 10

q = runState test_state 0
w = runWriter test_log

test_writerstate = do
  q <- lift $ add_state 2
  log_something (0,["Hello, "])
  w <- lift $ add_state 5
  log_something (5,["World."])
  e <- lift $ add_state 10
  return (q,w,e)

e = runState (runWriterT test_writerstate) 0

main = print e
