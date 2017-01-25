{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Control.Monad.Writer
import Control.Monad.State

-- So state and writer are polymorphic. That means I do
-- not have to explicitly differentiate the standard and
-- transformer versions.
add_state x = state $ \state -> (x + state, state)
log_something x = writer x

-- And this works without explicit lifting.
test_writerstate = do
  q <- add_state 2
  log_something (0,["Hello, "])
  w <- add_state 5
  log_something (5,["World."])
  e <- add_state 13
  return (q,w,e)

e = runState (runWriterT test_writerstate) 0
-- (((2,5,13),["Hello, ","World."]),0)
w = runWriter $ runStateT test_writerstate 0
-- (((2,5,13),0),["Hello, ","World."])

main = print "Hello"
