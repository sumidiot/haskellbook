module Sec7p3 where

{-
   Exercises: Grab Bag
   =================== -}

{- Ex. 1 - which are equivalent? -}
mTh1 x y z = x * y * z
mTh2 x y = \z -> x * y * z
mTh3 x = \y -> \z -> x * y * z
mTh4 = \x -> \y -> \z -> x * y * z

-- Answer: They are effectively equivalent.


{- Ex. 2 - which is type of mTh 3? -}
-- Answer: Num a => a -> a -> a 
-- That is, one fewer argument.

{- Ex. 3 - anonymous syntax -}
 -- a
addOneIfOdd n = case odd n of
   True -> f n 
   False -> n
   where f = \n -> n + 1

 -- b
addFive = \x -> \y -> (if x > y then y else x) + 5

 -- c
mflip f x y = f y x
   -- try: mflip (-) 2 3

