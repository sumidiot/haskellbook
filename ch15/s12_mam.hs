module MaybeAnotherMonoid where

import Data.Monoid
import Test.QuickCheck


-- from section 15.10

data Optional a =
    Nada
  | Only a
  deriving (Eq, Show)

-- from 15.12

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

-- from the exercise setup

newtype First' a =
  First' { getFirst' :: Optional a }
  deriving (Eq, Show)

instance Monoid (First' a) where
  mempty = First' Nada
  mappend (First' Nada) x = x
  mappend x@(First' (Only a)) _ = x


firstMappend :: First' a -> First' a -> First' a
firstMappend = mappend

type FirstMappend = First' String -> First' String -> First' String -> Bool
type FstId = First' String -> Bool

-- still trying to really appreciate Arbitrary and Gen
-- i see the utitlity, but haven't committed to memory how to do anything with them
-- :t arbitrary :: Arbitrary a => Gen a
-- :t frequency
genFirst' :: Arbitrary a => Gen (First' a)
genFirst' = do
  a <- arbitrary
  frequency [ (1, return (First' Nada))
            , (5, return (First' $ Only a)) ]

instance Arbitrary a => Arbitrary (First' a) where
  arbitrary = genFirst'


main :: IO ()
main = do
  quickCheck (monoidAssoc :: FirstMappend)
  quickCheck (monoidLeftIdentity :: FstId)
  quickCheck (monoidRightIdentity :: FstId)
