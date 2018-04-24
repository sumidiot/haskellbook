module ChapterExercises where

import Data.Char

-- Data.Char exercises

-- Exercise 1
-- :t isUpper
-- :t toUpper

-- Exercise 2
uppersInString :: String -> String
uppersInString = filter isUpper

-- Exercise 3
upperFirst :: String -> String
upperFirst "" = ""
--upperFirst s = (toUpper $ head s) : (tail s)
upperFirst (c : s) = (toUpper c) : s

-- Exercise 4
upperAll :: String -> String
upperAll "" = ""
upperAll (c : s) = (toUpper c) : (upperAll s)

-- Exercise 5
upperFirst' :: String -> Char
upperFirst' = toUpper . head

-- Exercise 6
-- whoops, did it as the first version


-- Ciphers exercises

-- this solution doesn't use the ord/chr hint of the exercise
-- it builds up mappings from input character to output character
-- and then basically applies that mapping to each character
--
-- i'd anticipate this being slow, because we're effectively applying
-- a function by doing a table scan for each input
cipher :: Int -> String -> String
cipher n s = map (charCipher n) s
  where charCipher n x = snd (head (filter (\t -> fst t == x) (cmap n)))
        cmapi = ' ' : ['a'..'z'] ++ ['A'..'Z']
        cmapo = ' ' : rot n ['a'..'z'] ++ rot n ['A'..'Z']
        rot n xs = (drop n xs) ++ (take n xs)
        cmap n = zip cmapi cmapo


-- Write your own standard functions

-- Exercise 1
myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs) = x || myOr xs

-- Exercise 2
-- not sure what's better here, recusion or compose with map
myAny :: (a -> Bool) -> [a] -> Bool
myAny f = myOr . (map f)
--myAny _ [] = False
--myAny f (x:xs) = f x || myAny f xs

-- Exercise 3
myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem a (x:xs) = (a == x) || (myElem a xs)

myElem' :: Eq a => a -> [a] -> Bool
myElem' a = any (== a)

-- Exercise 4
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = (myReverse xs) ++ [x]

-- Exercise 5
squish :: [[a]] -> [a]
squish [] = []
squish (xs:yss) = xs ++ (squish yss)

-- Exercise 6
squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = squish . (map f)

-- Exercise 7
squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

-- Exercise 8
myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy _ (a:[]) = a
myMaximumBy f (a:as) = maxOf f a (myMaximumBy f as)
  where maxOf f l r = case f l r of
                           LT -> r
                           EQ -> l
                           GT -> l

-- Exercise 9
myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f = myMaximumBy (r f)
  where r f a b = case f a b of
                           LT -> GT
                           EQ -> EQ
                           GT -> LT

-- Exercise 10
myMaximum :: (Ord a) => [a] -> a
myMaximum = myMaximumBy compare

myMinimum :: (Ord a) => [a] -> a
myMinimum = myMinimumBy compare

