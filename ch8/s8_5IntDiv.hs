module IntegralDivision where

dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
  where go n d count
         | n < d = (count, n) -- base case
         | otherwise = go (n - d) d (count + 1) -- recursive case

