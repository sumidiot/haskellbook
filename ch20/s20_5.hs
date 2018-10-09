{-# LANGUAGE FlexibleInstances #-}

module S20_5_Exercises where

import Data.Monoid -- for Sum

-- implement the functions in terms of foldMap or foldr

-- exercise 1
sum' :: (Foldable t, Num a) => t a -> a
sum' = getSum . foldMap Sum


-- exercise 2
product' :: (Foldable t, Num a) => t a -> a
product' = getProduct . foldMap Product


-- exercise 3
elem' :: (Foldable t, Eq a) => a -> t a -> Bool
elem' a = getAny . foldMap (\x -> Any (x == a))


-- exercise 5
{-#
data Max t = Max { getMax :: t }
instance Ord t => Semigroup (Max t) where
  (<>) a b = Max $ max (getMax a) (getMax b)
instance Ord t => Monoid (Max t) where
  mempty = undefined -- this makes things fail, same reason we're trying to return Maybe
maximum' :: (Foldable t, Ord a) => t a -> Maybe a
maximum' as = case null as of
                True -> Nothing
                False -> Just $ getMax $ foldMap Max as
#-}


-- fixed version of the commented block above
data Max t = Max { getMax :: t }
instance Ord t => Semigroup (Max t) where
  (<>) a b = Max $ max (getMax a) (getMax b)
instance Ord t => Monoid (Max (Maybe t)) where -- this requires FlexibleInstances
  mempty = Max Nothing
maximum' :: (Foldable t, Ord a) => t a -> Maybe a
maximum' = getMax . foldMap (Max . Just)


-- trying too hard
maximum'' :: (Foldable t, Ord a) => t a -> Maybe a
maximum'' as = case getFirst $ foldMap (First . Just) as of
                 Nothing -> Nothing
                 Just h -> Just (foldr max h as)


-- solution identified during meetup, with foldr
maximum''' :: (Foldable t, Ord a) => t a -> Maybe a
maximum''' = foldr f Nothing where
               f a Nothing   = Just a
               f a (Just a') = Just (max a a')


-- exercise 6
null' :: (Foldable t) => t a -> Bool
null' = getAny . foldMap (\x -> Any True)


-- exercise 7
length' :: (Foldable t) => t a -> Int
length' = getSum . foldMap (\x -> Sum 1)


-- exercise 8
toList' :: (Foldable t) => t a -> [a]
toList' = foldMap (\x -> [x])


-- exercise 9
fold' :: (Foldable t, Monoid m) => t m -> m
fold' = foldMap id


-- exercise 10
foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap' f as = foldr (\a -> \b -> f a <> b) mempty as
