module DiamondProblem2 where

class B a where
  f :: a -> (a, a)

instance B Int where
  f x = (x, 2 * x)

