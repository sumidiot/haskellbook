module Main where

import Test.QuickCheck
import Data.List (sort) -- ex2


-- exercise 1
half :: Float -> Float
half x = x / 2
halfIdentity = (*2) . half

prop_twiceHalfIsWhole = \x -> halfIdentity x == id x

ex1 :: IO ()
ex1 = quickCheck prop_twiceHalfIsWhole


-- exercise 2
listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs =
  snd $ foldr go (Nothing, True) xs
  where go _ status@(_, False) = status
        go y (Nothing, t) = (Just y, t)
        go y (Just x, t) = (Just y, x >= y)

