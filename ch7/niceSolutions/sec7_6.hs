
{- 7.6 Artful Dodgy -}

-- Fill in the types
dodgy :: Num a => a -> a -> a -- added
dodgy x y = x + y * 10
oneIsOne :: Num a => a -> a -- added
oneIsOne = dodgy 1
oneIsTwo :: Num a => a -> a -- added
oneIsTwo = (flip dodgy) 2

-- 1) -- The book has the answer
-- 2) dodgy 1 1 => 11
-- 3) dodgy 2 2 => 22
-- 4) dodgy 1 2 => 21
-- 5) dodgy 2 1 => 12
-- 6) oneIsOne 1 => 11
-- 7) oneIsOne 2 => 21
-- 9) oneIsTwo 1 => 21
-- 10) oneIsTwo 2 => 22
-- 11) oneIsTwo 3 => 23

-- run this to see:
answer :: [Integer]
answer = [
 (dodgy 1 1), (dodgy 2 2),
 (dodgy 1 2), (dodgy 2 1),
 (oneIsOne 1), (oneIsOne 2),
 (oneIsTwo 1), (oneIsTwo 2),
 (oneIsTwo 3)
 ] 
