module Sec7p4 where

{- Exercises: Variety Pack
   ======================= -}

-- 1)
k (x,y) = x
k1 = k ((4-1), 10)
k2 = k ("three", (1+2))
k3 = k (3,True)

-- a) What type is k?
--    k :: (a,b) -> a
--
-- b) What is the type of k2?
--    The type of the first argument passed to k,
--    that is, k2 :: [Char]
--
--    Is the same as k1 or k3?
--    No.  k1 :: Integer
--         k3 :: Integer

-- 2) Fill in definitions
f :: (a,b,c) -> (d,e,f) -> ((a,d), (c,f))
-- i.e., we take two 3-member tuples
--       and return a nested tuple with some of the values.
f (a,b,c) (d,e,f) = ((a,d), (c,f)) 
-- e.g., f (1,2,3) (4,5,6) returns ((1,4), (3,6))
