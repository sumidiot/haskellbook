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

--instance Traversable n => Traversable (S n) where
  --traverse :: Applicative f => (a -> f b) -> (S n a) -> f (S n b)
  --traverse f (S na a) = f a

testS = do
  quickBatch $ functor (undefined :: S [] (Int, Int, Int)) -- fails!!
  --sample' (arbitrary :: Gen (S [] Int))
