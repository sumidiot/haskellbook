module SemigroupExercises where

-- note that I only half did the exercises, mostly just providing instances, not QuickCheck stuff

-- for testing: stack ghci --package semigroups --package QuickCheck

import Data.Semigroup
import Test.QuickCheck

-- exercise 1
data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _ = Trivial

instance Arbitrary Trivial where
  arbitrary = return Trivial

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

type TrivAssoc = Trivial -> Trivial -> Trivial -> Bool

main1 :: IO ()
main1 =
  quickCheck (semigroupAssoc :: TrivAssoc)


-- exercise 2

newtype Identity a = Identity a deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where
  (Identity a) <> (Identity a') = Identity (a <> a')

--genId :: Arbitrary a => Gen (Identity a)
--genId = do
--  a <- arbitrary
--  return $ Identity a
instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return $ Identity a

instance Arbitrary a => Arbitrary (Sum a) where
  arbitrary = do
    a <- arbitrary
    return $ Sum a

type IdStringAssoc = Identity String -> Identity String -> Identity String -> Bool
type IdIntSumAssoc = Identity (Sum Int) -> Identity (Sum Int) -> Identity (Sum Int) -> Bool

main2 :: IO ()
main2 = do
  quickCheck (semigroupAssoc :: IdStringAssoc)
  quickCheck (semigroupAssoc :: IdIntSumAssoc)


-- exercise 3 (bored of QuickCheck)
data Two a b = Two a b
instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (Two a b) <> (Two a' b') = Two (a <> a') (b <> b')


-- exercise 4 and exercise 5 are repetitive

-- exercise 6
newtype BoolConj = BoolConj Bool
instance Semigroup BoolConj where
  (BoolConj True) <> (BoolConj True) = BoolConj True
  _ <> _                              = BoolConj False

-- exercise 7
newtype BoolDisj = BoolDisj Bool
instance Semigroup BoolDisj where
  (BoolDisj True) <> _ = BoolDisj True
  _ <> (BoolDisj True) = BoolDisj True
  _ <> _               = BoolDisj False


-- exercise 8
data Or a b = Fst a | Snd b
instance Semigroup (Or a b) where
  x@(Snd _) <> _ = x
  _ <> x@(Snd _) = x
  (Fst _) <> y@(Fst b) = y


-- exercise 9
-- setting up tests for this is probably a good exercise
newtype Combine a b = Combine { unCombine :: (a -> b) }
instance Semigroup b => Semigroup (Combine a b) where
  (Combine f) <> (Combine g) = Combine $ \a -> (f a) <> (g a)


-- exercise 10
newtype Comp a = Comp { unComp :: (a -> a) }
instance Semigroup (Comp a) where
  (Comp f) <> (Comp g) = Comp (f . g)

