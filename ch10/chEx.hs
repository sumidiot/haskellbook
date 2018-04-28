module ChapterExercises where

-- Warm-up and review

stops = "pbtdkg"
vowels = "aeiou"

-- 1.a make all possible stop-vowel-stop 3-tuples
mkTuples :: [a] -> [b] -> [(a,b,a)]
mkTuples as bs = [ (a,b,a') | a <- as, b <- bs, a' <- as ]

-- 1.b filtered to begin with a p
mkPTuples :: String -> [b] -> [(Char,b,Char)]
mkPTuples as bs = [ (a,b,a') | a <- filter (=='p') as, b <- bs, a' <- as]

-- 1.c already done as solution 1.a above

-- 2
seekritFunc x = div (sum (map length (words x))) (length (words x))
-- this function computes the floor of the average word length in a block of text
-- e.g., seekrit "this is lots of short words"
--       seektir "extemporaneously expressing verbose verbiage"

-- 3 re-write #2 using fractional division
seekritFunc' x = (/) (fromIntegral $ sum (map length (words x))) (fromIntegral $ length (words x))


-- Rewriting functions using folds

-- 1. myOr
myOr :: [Bool] -> Bool
myOr = foldr (||) False

-- 2. myAny
myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr (\a b -> b || f a) False
-- I really don't like this, because I don't understand why myAny [>5] [6]++[undefined] fails

-- 3. myElem
myElem :: Eq a => a -> [a] -> Bool
myElem a = myAny (==a)

-- 4. myReverse
myReverse :: [a] -> [a]
myReverse = foldr (\a b -> b ++ [a]) []

-- 5. myMap
myMap :: (a -> b) -> [a] -> [b]
--myMap f = foldr (\a b -> f a : b) []
myMap f = foldr ((:) . f) []

-- 6. myFilter
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter p = foldr maybeConcat []
  where maybeConcat a bs
                  | p a       = a : bs
                  | otherwise = bs

-- 7. squish
squish :: [[a]] -> [a]
squish = foldr (++) []

-- 8. squishMap
squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr ((++) . f) []

-- 9. squishAgain
squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

-- 10. myMaximumBy
myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy c as = foldl larger (head as) as -- wanted foldr here, but it makes the second test case "incorrect"
  -- note, though, that \_ _ -> LT is a weird ordering (so is -> GT i guess)
  where larger a b
             | c a b == GT = a
             | c a b == LT = b
             | otherwise   = b

-- 11. myMinimumBy
myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy c as = foldl smaller (head as) as -- goofy, as in #10
  where smaller a b
              | c a b == LT = a
              | c a b == GT = b
              | otherwise   = a
