module S9_5EnumFromTo where

eftBool :: Bool -> Bool -> [Bool]
eftBool True True = [True]
eftBool True False = []
eftBool False True = [False,True]
eftBool False False = [False]

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd f t = reverse $ go f t []
  where go f t l
           | f > t = l
           | f == t = (f : l)
           | f < t = go (succ f) t (f : l)
-- this is interesting... i had originally bundled the '==' case in with the '<',
-- but it fails if `succ` is a partial function, which it is for Ordering.
-- Yes, Haskell's lazy, so (succ f) isn't computed immediately, but it is required
-- to be computed to check the guards on the subsequent go.
--
-- the implementation here uses Eq, Ord, and Enum (for succ), so works for all
-- the types that this exercise targets (but I guess using Enum is cheating?)

eft :: (Eq a, Ord a, Enum a) => a -> a -> [a]
eft f t = reverse $ go f t []
  where go f t l
           | f > t = l
           | f == t = (f : l)
           | f < t = go (succ f) t (f : l)

