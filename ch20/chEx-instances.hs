module Instances where


-- exercise 1
data Constant a b = Constant b
instance Foldable (Constant a) where
  foldMap f (Constant b) = f b


-- exercise 2
data Two a b = Two a b
instance Foldable (Two a) where
  foldMap f (Two a b) = f b


-- exercise 3
data Three a b c = Three a b c
instance Foldable (Three a b) where
  foldMap f (Three a b c) = f c


-- exercise 4
data Three' a b = Three' a b b
instance Foldable (Three' a) where
  foldMap f (Three' a b b') = f b <> f b' -- could also be f b, or f b'


-- exercise 5
data Four' a b = Four' a b b b
instance Foldable (Four' a) where
  foldMap f (Four' a b b' b'') = (f b <> f b') <> f b''
