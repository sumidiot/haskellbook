module ChapterExercises where

-- for the final exercise
import Data.List (intersperse)
import Data.Char (toLower)

cattyConny :: String -> String -> String
cattyConny x y = x ++ " mrow " ++ y

flippy = flip cattyConny

appedCatty = cattyConny "woops" -- appedCatty x = "woops mrow x"
frappe = flippy "haha" -- frappe x = "x mrow haha"


-- Recursion exercise #2
triang :: (Eq a, Num a) => a -> a
triang 0 = 0
triang n = n + triang (n - 1)


-- Recursion exercise #3
sumMult :: (Integral a) => a -> a -> a
sumMult 0 _ = 0
sumMult n x = x + sumMult (n - 1) x


-- Fixing dividedBy
dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
  where go n d count
         | n < d = (count, n) -- base case
         | otherwise = go (n - d) d (count + 1) -- recursive case

-- if the goal is to write divMod, this isn't it, because divMod 10 (-3) = (-4,-2), mine is (-3, 1)
dividedBy' :: (Integral a, Num a) => a -> a -> (a, a)
dividedBy' num denom = go num denom 0
  where go n d count
         | n < 0,
           let (q, m) = go (-n) d count
           = (-q, m)
         | d < 0,
           let (q, m) = go n (-d) count
           = (-q, m)
         | n < d = (count, n) -- base case
         | otherwise = go (n - d) d (count + 1) -- recursive case

data DividedResult a = Result (a, a) | DividedByZero deriving Show
dividedBy'' :: (Integral a, Num a) => a -> a -> DividedResult a
dividedBy'' num 0 = DividedByZero
dividedBy'' num denom = Result (go num denom 0)
  where go n d count
         | n < 0,
           let (q, m) = go (-n) d count
           = (-q, m)
         | d < 0,
           let (q, m) = go n (-d) count
           = (-q, m)
         | n < d = (count, n) -- base case
         | otherwise = go (n - d) d (count + 1) -- recursive case


-- The McCarthy 91 Function
mc91 :: Integral a => a -> a
mc91 n
  | n > 100   = n - 10
  | otherwise = mc91 (mc91 (n + 11))


-- Numbers into words
-- this solution doesn't have all the nice typesafety we should aim for, with negatives/zeros
digitToWord :: Int -> String
digitToWord n =
  case n of
    0 -> "zero"
    1 -> "one"
    2 -> "two"
    3 -> "three"
    4 -> "four"
    5 -> "five"
    6 -> "six"
    7 -> "seven"
    8 -> "eight"
    9 -> "nine"

digits :: Int -> [Int]
digits n
  | n < 10 = [n]
  | otherwise,
    let (d,m) = divMod n 10
    = (digits d) ++ [m]

wordNumber :: Int -> String
wordNumber n = concat (intersperse "-" (map digitToWord (digits n)))


data Digit = Zero | One | Two | Three | Four | Five | Six | Seven | Eight | Nine deriving Show
digits' :: Int -> [Digit]
digits' n
  | n == 0 = [Zero]
  | n == 1 = [One]
  | n == 2 = [Two]
  | n == 3 = [Three]
  | n == 4 = [Four]
  | n == 5 = [Five]
  | n == 6 = [Six]
  | n == 7 = [Seven]
  | n == 8 = [Eight]
  | n == 9 = [Nine]
  | otherwise,
    let (d,m) = divMod n 10
    = (digits' d) ++ (digits' m)

digitToWord' :: Digit -> String
digitToWord' = (map toLower) . show

wordNumber' :: Int -> String
wordNumber' n
  | n < 0 = "negative-" ++ wordNumber' (-n)
  | otherwise = concat (intersperse "-" (map digitToWord' (digits' n)))
