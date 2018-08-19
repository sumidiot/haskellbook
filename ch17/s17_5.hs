module Section17_5 where

import Control.Applicative
import Data.List (elemIndex)

-- Lookups exercises, p.1076

-- exercise 1
added :: Maybe Integer
added = (+3) <$> (lookup 3 $ zip [1, 2, 3] [4, 5, 6]) -- added <$>

-- exercise 2
y :: Maybe Integer
y = lookup 3 $ zip [1, 2, 3] [4, 5, 6]
z :: Maybe Integer
z = lookup 2 $ zip [1, 2, 3] [4, 5, 6]
tupled :: Maybe (Integer, Integer)
tupled = liftA2 (,) y z -- added liftA2, so this is (,) <$> y <*> z

-- exercise 3
x3 :: Maybe Int
x3 = elemIndex 3 [1, 2, 3, 4, 5]
y3 :: Maybe Int
y3 = elemIndex 4 [1, 2, 3, 4, 5]
max' :: Int -> Int -> Int
max' = max
maxed :: Maybe Int
maxed = liftA2 max' x3 y3 -- added liftA2

-- exercise 4
xs = [1, 2, 3]
ys = [4, 5, 6]
x4 :: Maybe Integer
x4 = lookup 3 $ zip xs ys
y4 :: Maybe Integer
y4 = lookup 2 $ zip xs ys
--summed :: Maybe Integer
--summed = sum $ (,) x4 y4
-- given that i only have 2 things, i could do liftA2 (+) x4 y4
-- if i assumed i could have more Maybe Integers, list a list of them, and wanted to sum that list
-- i'd have a [Maybe Integer], sum :: [Integer] -> Integer
-- i could imagine two meanings: (1) if any Nothings, Nothing, (2) sum of all Justs
-- the book is clearly suggesting (,) as the Foldable for sum, though it's a silly foldable (takes the second)
--
-- really not sure what's going on here
--
-- a way to do something reasonable is
--   convert the Integers to Sum, to specify that monoid on Int
--   mconcat the [Maybe Sum], which will create Maybe Sum of all the Justs
--   extract the Sum back out as an Int (fmap getsum)
-- getSum <$> (mconcat $ (fmap . fmap) Sum [x4, y4])



-- Identity section, p.1079
-- applicative instance for Identity
newtype Identity a = Identity a deriving (Eq, Ord, Show)
instance Functor Identity where
  fmap f (Identity a) = Identity (f a)
instance Applicative Identity where
  pure a = Identity a
  (<*>) (Identity f) (Identity a) = Identity (f a)


-- Constant section, p.1082
newtype Constant a b = Constant { getConstant :: a } deriving (Eq, Ord, Show)
instance Functor (Constant a) where
  fmap f c = Constant (getConstant c)
instance Monoid a => Applicative (Constant a) where
  pure b = Constant mempty
  (<*>) (Constant a) c = Constant (a `mappend` getConstant c)
