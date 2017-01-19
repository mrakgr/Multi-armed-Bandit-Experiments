{-# LANGUAGE ScopedTypeVariables #-}

import Control.Monad
import qualified Data.HashTable.IO as H
import System.TimeIt

type HashTable k v = H.BasicHashTable k v

main =
  timeIt $ do
    (m :: HashTable Int Int) <- H.new
    forM_ [1..10000000] $ \n -> H.insert m n n
    v <- H.lookup m 100
    print v
