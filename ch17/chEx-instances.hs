module Instances where

import Control.Applicative
import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes


-- exercise 1
data Pair a = Pair a a deriving Show
instance Functor Pair where
  fmap f (Pair a a') = Pair (f a) (f a')
instance Applicative Pair where
  pure a = Pair a a
  (<*>) (Pair f f') (Pair a a') = Pair (f a) (f' a')


-- exercise 2
data Two a b = Two a b
instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)
instance Monoid a => Applicative (Two a) where
  pure b = Two mempty b
  (<*>) (Two a fb) (Two a' b') = Two (a <> a') (fb b')


-- exercise 3
data Three a b c = Three a b c
instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)
instance (Monoid a, Monoid b) => Applicative (Three a b) where
  pure c = Three mempty mempty c
  (<*>) (Three a b fc) (Three a' b' c') = Three (a <> a') (b <> b') (fc c')


-- exercise 4
data Three' a b = Three' a b b
instance Functor (Three' a) where
  fmap f (Three' a b b') = Three' a (f b) (f b')
instance (Monoid a) => Applicative (Three' a) where
  pure b = Three' mempty b b
  (<*>) (Three' a fb fb') (Three' a' b b') = Three' (a <> a') (fb b) (fb' b')


-- exercise 5
data Four a b c d = Four a b c d deriving (Eq, Show)
instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)
instance (Monoid a, Monoid b, Monoid c) => Applicative (Four a b c) where
  pure d = Four mempty mempty mempty d
  (<*>) (Four a b c fd) (Four a' b' c' d) = Four (a <> a') (b <> b') (c <> c') (fd d)

instance (Eq a, Eq b, Eq c, Eq d) => EqProp (Four a b c d) where (=-=) = eq
fourGen :: (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Gen (Four a b c d)
fourGen = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  d <- arbitrary
  return $ Four a b c d
instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = fourGen

checkFour = do
  quickBatch $ applicative (undefined :: Four [String] [String] [String] (Int, Int, Int))
-- wanted to do things like (Sum Int), but no Arbitrary for them



-- exercise 6
data Four' a b = Four' a a a b deriving (Eq, Show)
instance Functor (Four' a) where
  fmap f (Four' a a' a'' b) = Four' a a' a'' (f b)
instance Monoid a => Applicative (Four' a) where
  pure b = Four' mempty mempty mempty b
  (<*>) (Four' a1 a2 a3 fb) (Four' a1' a2' a3' b) = Four' (a1 <> a1') (a2 <> a2') (a3 <> a3') (fb b)

instance (Eq a, Eq b) => EqProp (Four' a b) where (=-=) = eq
fourGen' :: (Arbitrary a, Arbitrary b) => Gen (Four' a b)
fourGen' = do
  a <- arbitrary
  a' <- arbitrary
  a'' <- arbitrary
  b <- arbitrary
  return $ Four' a a' a'' b
instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = fourGen'

checkFour' = do
  quickBatch $ applicative (undefined :: Four' [String] (Int, Int, Int))
