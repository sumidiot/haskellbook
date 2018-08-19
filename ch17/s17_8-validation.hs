module ValidationApplicative where

import Control.Applicative
import Data.Monoid
import qualified Test.QuickCheck as QC
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes


data Validation e a = Failure e | Success a
  deriving (Eq, Show)

instance (Eq e, Eq a) => EqProp (Validation e a) where (=-=) = eq
valGen :: (QC.Arbitrary e, QC.Arbitrary a) => QC.Gen (Validation e a)
valGen = do
  e <- QC.arbitrary
  a <- QC.arbitrary
  QC.oneof [return $ Failure e, return $ Success a]
instance (QC.Arbitrary e, QC.Arbitrary a) => QC.Arbitrary (Validation e a) where
  arbitrary = valGen

instance Functor (Validation e) where
  fmap _ (Failure e) = Failure e
  fmap f (Success a) = Success $ f a

instance Monoid e => Applicative (Validation e) where
  pure a = Success a
  (<*>) (Success f) (Success a) = Success $ f a
  (<*>) (Success f) (Failure e) = Failure e
  (<*>) (Failure e) (Success a) = Failure e
  (<*>) (Failure e) (Failure a) = Failure (e <> a)

main = do
  quickBatch $ applicative (undefined :: Validation [String] (Int, Int, Int))

