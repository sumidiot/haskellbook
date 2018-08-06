module ValidInstances where

import GHC.Arr

-- exercise 1
data Bool = False | True
-- can't be a Functor because it has kind *, not * -> *


-- exercise 2
-- this type is the same as Either a a
data BoolAndSomethingElse a = False' a | True' a
instance Functor BoolAndSomethingElse where
  fmap f (False' a) = False' $ f a
  fmap f (True'  a) = True'  $ f a


-- exercise 3
-- this type is the same as Maybe a
data BoolAndMaybeSomethingElse a = Falsish | Trueish a
instance Functor BoolAndMaybeSomethingElse where
  fmap f Falsish     = Falsish
  fmap f (Trueish a) = Trueish $ f a


-- exercise 4
newtype Mu f = InF { outF :: f (Mu f) }
-- f has kind * -> *, because it applied to (Mu f) in outF


-- exercise 5
data D = D (Array Word Word) Int Int
-- kind *

