module Instances where

import Control.Applicative
import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-- Identity
newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  --fmap :: (a -> b) -> Identity a -> Identity b
  fmap f (Identity a) = Identity $ f a

instance Foldable Identity where
  --foldMap :: Monoid m => (a -> m) -> Identity a -> m
  foldMap f (Identity a) = f a

instance Traversable Identity where
  --traverse :: Applicative f => (a -> f b) -> Identity a -> f (Identity b)
  traverse f (Identity a) = fmap Identity $ f a

instance Eq a => EqProp (Identity a) where (=-=) = eq
instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return $ Identity a


testIdentity = do
  quickBatch $ functor (undefined :: Identity (Int, Int, Int))
  quickBatch $ traversable (undefined :: Identity (Int, Int, [Int]))




-- S (state?)
data S n a = S (n a) a deriving (Eq, Show)

instance (Functor n, Arbitrary (n a), Arbitrary a) => Arbitrary (S n a) where
  arbitrary = S <$> arbitrary <*> arbitrary

instance (Applicative n, Testable (n Property), EqProp a) => EqProp (S n a) where
  (S x y) =-= (S p q) = (property $ (=-=) <$> x <*> p) .&. (y =-= q)

instance Functor n => Functor (S n) where
  -- fmap :: (a -> b) -> S n a -> S n b
  fmap f (S na a) = S (f <$> na) (f a) -- -- this compiles but fails the laws!

instance Foldable (S n) where
  --foldMap :: Monoid m => (a -> m) -> S n a -> m
  foldMap f (S na a) = f a

instance Traversable n => Traversable (S n) where
  --traverse :: Applicative f => (a -> f b) -> (S n a) -> f (S n b)
  traverse g (S na a) = liftA2 S (traverse g na) (g a)
  -- na is an (n a), and n is traversable, so (traverse g na) is an f (n b)
  -- (g a) is an f b
  -- to create an S n b, I need an n b and a b, and I have those, they're just wrapped in f
  -- but f is applicative, so I can liftA2 S, and get a map (f (n b)) -> (f b) -> f (S n b)

testS = do
  --quickBatch $ functor (undefined :: S [] (Int, Int, Int)) -- fails!!
  --sample' (arbitrary :: Gen (S [] Int))
  quickBatch $ traversable (undefined :: S [] ([Int], [Int], [Int])) -- also fails!!




data Tree a = Empty | Leaf a | Node (Tree a) a (Tree a) deriving (Eq, Show)

instance Functor Tree where
  -- fmap :: (a -> b) -> Tree a -> Tree b
  fmap _ Empty = Empty
  fmap f (Leaf a) = Leaf (f a)
  fmap f (Node ta a ta') = Node (fmap f ta) (f a) (fmap f ta')

instance Foldable Tree where
  -- foldMap :: Monoid m => (a -> m) -> Tree a -> m
  foldMap _ Empty = mempty
  foldMap f (Leaf a) = f a
  foldMap f (Node ta a ta') = (foldMap f ta) <> (f a) <> (foldMap f ta')

instance Traversable Tree where
  traverse g Empty = pure Empty
  traverse g (Leaf a) = Leaf <$> g a
  traverse g (Node l a r) = liftA3 Node (traverse g l) (g a) (traverse g r)


instance Eq a => EqProp (Tree a) where (=-=) = eq
instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary = do
    a <- arbitrary
    l <- arbitrary
    r <- arbitrary
    frequency [(3, return Empty), (2, return $ Leaf a), (1, return $ Node l a r)]


testTree = do
  quickBatch $ functor (undefined :: Tree (Int, Int, Int))
  quickBatch $ traversable (undefined :: Tree ([Int], [Int], [Int]))

