module ChExMatchTheTypes where

import Data.List

i1a :: Num a => a
i1a = 1
--i1b :: a -- fails, this type signature doesn't make sense
i1b = 1

f2a :: Float
f2a = 1.0
--f2b :: Num a => a -- 1.0 is Fractional, where Num is broader than that
f2b = 1.0

f3a :: Float
f3a = 1.0
f3b :: Fractional a => a
f3b = 1.0

f4a :: Float
f4a = 1.0
f4b :: RealFrac a => a
f4b = 1.0

freudA :: a -> a
freudA x = x
freudB :: Ord a => a -> a
freudB x = x

freudA' :: a -> a
freudA' x = x
freudB' :: Int -> Int
freudB' x = x

myX = 1 :: Int
sigmundA :: Int -> Int
sigmundA x = myX
--sigmundB :: a -> a -- Fails, return type has to be Int
sigmundB x = myX

sigmundA' :: Int -> Int
sigmundA' x = myX
--sigmundB' :: Num a => a -> a -- Fails, still need Int
sigmundB' x = myX

jungA :: Ord a => [a] -> a
jungA xs = head (sort xs)
jungB :: [Int] -> Int
jungB xs = head (sort xs)

youngA :: [Char] -> Char
youngA xs = head (sort xs)
youngB :: Ord a => [a] -> a
youngB xs = head (sort xs)

mySort :: [Char] -> [Char]
mySort = sort
signifierA :: [Char] -> Char
signifierA xs = head (mySort xs)
--signifierB :: Ord a => [a] -> a -- Fails, has to be `Char`
signifierB xs = head (mySort xs)
