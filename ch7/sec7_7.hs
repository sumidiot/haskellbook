{- 7.7 Guard Duty -}

-- 1) try otherwise first
avgGrade :: (Fractional a, Ord a) => a -> Char
avgGrade x 
 | otherwise = 'F' -- this will make all grades = 'F'
 |y>=0.9 ='A' 
 |y>=0.8 ='B' 
 |y>=0.7 ='C' 
 |y >= 0.59 = 'D'
 |y< 0.59='F' 
 where y = x / 100


-- 2) reorder guards

avgGrade2 :: (Fractional a, Ord a) => a -> Char
avgGrade2 x 
 |y>=0.9 ='A' 
 |y< 0.59='F' 
 |y>=0.7 ='C' 
 |y>= 0.59 = 'D'
 |y>=0.8 ='B' 
 where y = x / 100
-- It still runs.
--
-- Now 82% gives 'C' grade because it matches first
-- 
-- If you use
--    :set -Wall
-- in GHCi, it will warn on load about non-exhaustive 
-- patterns ( though they are...aren't they? :D ).


-- 3) Given the palindrome function below

pal xs
 | xs == reverse xs = True 
 | otherwise = False

-- it returns b) True when xs is a plaindrome

-- 4) What types of arguments can pal take?
--      pal :: Eq a => [a] -> Bool
--    That is, any kind of sequence of equatable items.

-- 5) What is the type of the function?
--    See above.

-- 6) The following function returns...?
numbers x
 | x < 0   = -1
 | x == 0  =  0 
 | x > 0   =  1

-- the "sign" of x 
--   (1 if positive, 0 if zero, -1 if negative)
-- Answer: c.

-- 7) What types of args can it take?
-- 8) What is its it type?
--  numbers :: (Ord a, Num a, Num p) => a -> p
-- So it can take any a numeric type that has
--  an relative order (e.g., a < 0 is defined).


