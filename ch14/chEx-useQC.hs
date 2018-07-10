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

sortIsOrdered :: (Ord a) => [a] -> Bool
sortIsOrdered = listOrdered . sort

-- I'm not sure how to make this do a thing like exercise 1
--ex2 :: IO ()
--ex2 = quickCheck sortIsOrdered


-- exercise 3
plusAssociative x y z = x + (y + z) == (x + y) + z
plusCommutative x y = x + y == y + x


-- exercise 4
multiplicationAssociative x y z = x * (y * z) == (x * y) * z
multiplicationCommutative x y = x * y == y * x


-- exercise 5
quotPlusRem x y = (quot x y)*y + (rem x y) == x
divPlusMod x y = (div x y)*y + (mod x y) == x

integralQuotientPair :: (Arbitrary a, Integral a) => Gen (a, NonZero a)
integralQuotientPair = do
  a <- arbitrary
  b <- arbitrary
  return (a, b)

quotPlusRemIQP :: Integral a => (a, a) -> Bool
quotPlusRemIQP = uncurry quotPlusRem


-- exercise 6
-- exponentiation is not associative or commutative
arrowIsAssoc x y z = x ^ (y ^ z) == (x ^ y) ^ z
arrowIsCommutative x y = x ^ y == y ^ x


-- exercise 7
reverseIsId :: Eq a => [a] -> Bool
reverseIsId x = (reverse . reverse) x == id x -- how to write this point-free?


-- exercise 8
functionApply :: Eq b => (a -> b) -> a -> Bool
functionApply f a = (f $ a) == f a
