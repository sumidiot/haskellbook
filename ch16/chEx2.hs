module WorkingFunctor where

-- exercise 1
data Sum b a = First a | Second b -- flipped a and b
instance Functor (Sum e) where
  fmap f (First a)  = First (f a)
  fmap f (Second b) = Second b


-- exercise 2
data Company a c b = DeepBlue a c | Something b -- moved b to end
instance Functor (Company e e') where
  fmap f (Something b)  = Something (f b)
  fmap _ (DeepBlue a c) = DeepBlue a c


-- exercise 3
data More b a = L a b a | R b a b deriving (Eq, Show) -- flipped a and b
instance Functor (More x) where
  fmap f (L a b a') = L (f a) b (f a')
  fmap f (R b a b') = R b (f a) b'

lp1 = fmap (+1) (L 1 2 3) -- should be L 2 2 4
rp1 = fmap (+1) (R 1 2 3) -- should be R 1 3 3

