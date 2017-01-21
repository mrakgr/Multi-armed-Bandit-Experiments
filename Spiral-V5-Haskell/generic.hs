{-# LANGUAGE ScopedTypeVariables #-}

module Main where

data Action =
  Fold | Call | RaiseOne
data Hand =
  Ace | King | Queen | Jack
data OpponentPreviousAction =
  ROne | Check

class NumStates x where
  num_states :: x -> Int

instance NumStates Action where
  num_states _ = 3
instance NumStates Hand where
  num_states _ = 4
instance NumStates OpponentPreviousAction where
  num_states _ = 2

main = print "Hello"
