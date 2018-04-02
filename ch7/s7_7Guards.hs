module Section7_7 where

bloodNa :: Integer -> String
bloodNa x
  | x < 135 = "lo"
  | x > 145 = "hi"
  | otherwise = "ok"

isRight :: (Num a, Eq a) => a -> a -> a -> String
isRight a b c
  | a^2 + b^2 == c^2 = "yep"
  | otherwise        = "nope"

avgGrade :: (Fractional a, Ord a) => a -> Char
avgGrade x
  | y >= 0.9 = 'A'
  | y >= 0.8 = 'B'
  | y >= 0.7 = 'C'
  | y >= 0.59 = 'D'
  | y < 0.59 = 'F'
  where y = x / 100

pal xs
  | xs == reverse xs = True
  | otherwise        = False

numbers x
  | x < 0  = -1
  | x == 0 = 0
  | x > 0  = 1

