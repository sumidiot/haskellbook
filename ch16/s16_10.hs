{-# LANGUAGE ViewPatterns #-}
module FunctorInstances where

-- from section 16.9, for QuickCheck helpers
import Test.QuickCheck
import Test.QuickCheck.Function

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Eq (f c), Functor f) => (a -> b) -> (b -> c) -> f a -> Bool
functorCompose f g x = (fmap g (fmap f x)) == (fmap (g . f) x)

functorCompose' :: (Eq (f c), Functor f) => f a -> Fun a b -> Fun b c -> Bool
functorCompose' x (Fun _ f) (Fun _ g) = (fmap (g . f) x) == (fmap g . fmap f $ x)


-- I know I should implement Gens for these types, so I can quickCheck...

-- exercise 1
newtype Identity a = Identity a deriving (Eq, Show)
instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return $ Identity a


-- exercise 2
data Pair a = Pair a a
instance Functor Pair where
  fmap f (Pair a a') = Pair (f a) (f a')


-- exercise 3
data Two a b = Two a b
instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)


-- exericse 4
data Three a b c = Three a b c
instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)


-- exercise 5
data Three' a b = Three' a b b
instance Functor (Three' a) where
  fmap f (Three' a b b') = Three' a (f b) (f b')


-- exercise 6
data Four a b c d = Four a b c d
instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)


-- exericse 7
data Four' a b = Four' a a a b
instance Functor (Four' a) where
  fmap f (Four' a a' a'' b) = Four' a a' a'' (f b)


-- exercise 8
data Trivial = Trivial
-- can't be a functor, has kind `*`. however,
data Trivial' a = Trivial'
instance Functor Trivial' where
  fmap f Trivial' = Trivial'
