{-# LANGUAGE RankNTypes #-}

module Main where
  g :: (forall a b. a -> b) -> (a, b, c) -> (e, f, g)
  g f (a, b, c) = (f a, f b, f c)

  main = print $ g show (2 :: Integer, 2.5 :: Double, True)
