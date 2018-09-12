module EitherMonad where

import Control.Applicative
--import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes


data Sum a b = First a | Second b deriving (Eq, Show)


instance Functor (Sum a) where
  fmap _ (First a) = First a
  fmap f (Second b) = Second (f b)


instance Applicative (Sum a) where
  pure b = Second b
  -- (<*>) :: (Sum a (a' -> b)) (Sum a a') -> (Sum a b)
  (<*>) (Second f) (Second b) = Second (f b)
  (<*>) (Second f) (First a)  = First a
  (<*>) (First  f) (First a)  = First a
  (<*>) (First  a) (Second b) = First a


instance Monad (Sum a) where
  return = pure
  -- (>>=) :: (Sum a a') -> (a' -> Sum a b) -> Sum a b
  (>>=) (First  a)  _ = First a
  (>>=) (Second a') f = f a'


instance (Eq a, Eq b) => EqProp (Sum a b) where (=-=) = eq
sumGen :: (Arbitrary a, Arbitrary b) => Gen (Sum a b)
sumGen = do
  a <- arbitrary
  b <- arbitrary
  frequency [(2, return $ First a), (3, return $ Second b)]

instance (Arbitrary a, Arbitrary b) => Arbitrary (Sum a b) where
  arbitrary = sumGen


checkFunctor = do
  quickBatch $ functor (undefined :: Sum Int (Int, Int, Int))

checkApplicative = do
  quickBatch $ applicative (undefined :: Sum Int (Int, Int, Int))

checkMonad = do
  quickBatch $ monad (undefined :: Sum Int (Int, Int, Int))


