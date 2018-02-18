module ChExWaTS where

--functionH :: [t] -> t
functionH (x:_) = x

--functionC :: Ord a => a -> a -> Bool
functionC x y =
  if (x > y) then True else False -- verbose way to say x > y

--functionS :: (t, t1) -> t1
functionS (x, y) = y

