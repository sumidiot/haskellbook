module Main where

import DiamondProblem2

class A a where
  f :: a -> (a, a)

--class B a where
--  f :: a -> (a, a)


instance A Int where
--  f :: Int -> (Int, Int)
  f x = (x, x)

--instance B String where
--  f :: Int -> (Int, Int)
--  f x = (x, x ++ " " ++ x)

--main :: IO ()
main = do
  print $ f (2 :: Int)
