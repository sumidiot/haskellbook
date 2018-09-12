module MonadInstances where

import Control.Applicative
import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-- exercise 1, Nope

data Nope a = NopeDotJpg deriving (Eq, Show)
instance Eq a => EqProp (Nope a) where (=-=) = eq
instance Arbitrary a => Arbitrary (Nope a) where
  arbitrary = return NopeDotJpg

instance Functor Nope where
  fmap _ _ = NopeDotJpg

instance Applicative Nope where
  pure _ = NopeDotJpg
  (<*>) _ _ = NopeDotJpg

instance Monad Nope where
  _ >>= _ = NopeDotJpg

checkNope = do
  quickBatch $ functor (undefined :: Nope (Int, Int, Int))
  quickBatch $ applicative (undefined :: Nope (Int, Int, Int))
  quickBatch $ monad (undefined :: Nope (Int, Int, Int))


-- exercise 2, Either

data Meither b a = MLeft a | MRight b deriving (Eq, Ord, Show)
instance (Eq a, Eq b) => EqProp (Meither b a) where (=-=) = eq
instance (Arbitrary a, Arbitrary b) => Arbitrary (Meither b a) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    oneof [ return $ MLeft a, return $ MRight b ]

instance Functor (Meither b) where
  fmap f (MLeft a) = MLeft (f a)
  fmap _ (MRight b) = MRight b

instance Applicative (Meither b) where
  pure = MLeft
  (<*>) (MLeft f) (MLeft a)    = MLeft (f a)
  (<*>) (MLeft f) (MRight b)   = MRight b
  (<*>) (MRight b) (MLeft a)   = MRight b -- for some reason this one always trips me up
  (<*>) (MRight b) (MRight b') = MRight b -- it doesn't seem to matter which b you take

instance Monad (Meither b) where
  return = pure
  (MLeft a) >>= f = f a
  (MRight b) >>= _ = MRight b

checkMeither = do
  quickBatch $ functor (undefined :: Meither Int (Int, Int, Int))
  quickBatch $ applicative (undefined :: Meither Int (Int, Int, Int))
  quickBatch $ monad (undefined :: Meither Int (Int, Int, Int))


-- exercise 3, Identity

newtype Identity a = Identity a deriving (Eq, Ord, Show)
instance Eq a => EqProp (Identity a) where (=-=) = eq
instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return $ Identity a

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
  pure = Identity
  (<*>) (Identity f) (Identity a) = Identity (f a)

instance Monad Identity where
  return = pure
  (Identity a) >>= f = f a

checkId = do
  quickBatch $ functor (undefined :: Identity (Int, Int, Int))
  quickBatch $ applicative (undefined :: Identity (Int, Int, Int))
  quickBatch $ monad (undefined :: Identity (Int, Int, Int))


-- exercise 4, List

data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Eq a => EqProp (List a) where (=-=) = eq
listGen :: Arbitrary a => Gen (List a)
listGen = do
  a <- arbitrary
  frequency [(1, return Nil), (10, (Cons a) <$> listGen)]
instance Arbitrary a => Arbitrary (List a) where
  arbitrary = listGen

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a as) = Cons (f a) (fmap f as)

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x $ xs `append` ys

fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil = b
fold f b (Cons h t) = f h (fold f b t)

concat' :: List (List a) -> List a
concat' = fold append Nil

flatMap :: (a -> List b) -> List a -> List b
flatMap f as = concat' $ fmap f as

instance Applicative List where
  pure a = Cons a Nil
  (<*>) _ Nil = Nil
  (<*>) Nil _ = Nil
  (<*>) lf la = flatMap (\f -> fmap f la) lf

-- need to provide >>= :: List a -> (a -> List b) -> List b
instance Monad List where
  Nil >>= _ = Nil
  (Cons a as) >>= f = append (f a) (as >>= f)

checkList = do
  quickBatch $ functor (undefined :: List (Int, Int, Int))
  quickBatch $ applicative (undefined :: List (Int, Int, Int))
  quickBatch $ monad (undefined :: List (Int, Int, Int))
