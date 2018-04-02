module Sec7p5 where

{- Exercises: "Case Practice" -}

-- 1. rewrite into case
functionC x y = if(x >y) then x else y
functionC_case x y = case (x>y) of
   True -> x
   False -> y

-- 2. rewrite
ifEvenAdd2 n = if even n then (n+2) else n
ifEvenAdd2_case n = case (even n) of 
    True -> n+2
    otherwise -> n -- we could use False

-- 3. handle all cases
nums x = case compare x 0 of
  LT -> -1
  EQ -> 0 -- added
  GT -> 1

