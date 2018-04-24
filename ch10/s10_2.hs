module S10_2 where

map' f as = foldr mc [] as
  where mc a b = (f a) : b

