module Meetup where

myAny :: (a -> Bool) -> [a] -> Bool
myAny f as = foldr (\a b -> f a || b) False as
--  where g a b = f a || b

myAny''' :: (a -> Bool) -> [a] -> Bool
myAny''' f xs =
  let g a b = f a || b
    in foldr g False xs

myAny' :: (a -> Bool) -> [a] -> Bool
myAny' f as = foldr g False as
  where g a b = b || f a

myAny'' :: (a -> Bool) -> [a] -> Bool
myAny'' f a = foldr g False a
  where g a b = if (f a) then True else b


myElem :: Eq a => a -> [a] -> Bool
myElem a as = myAny (==a) as

myElem' :: Eq a => a -> [a] -> Bool
myElem' x as = foldr g False as
  where g a b = a == x || b
