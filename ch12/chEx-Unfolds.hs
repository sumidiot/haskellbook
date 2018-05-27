module Unfolds where

-- exercise 1

myIterate :: (a -> a) -> a -> [a]
myIterate f s = s:(myIterate f (f s))


-- exercise 2

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f b = case f b of
  Nothing      -> []
  Just (a, b') -> a:(myUnfoldr f b')


-- exercise 3

betterIterate :: (a -> a) -> a -> [a]
betterIterate f a = myUnfoldr g a
  where g x = Just (x, f x)


-- I'm putting the "Finally something other than list!" exercises here


data BinaryTree a =
  Leaf | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

-- exercise 1

unfold :: (a -> Maybe (a,b,a)) -> a -> BinaryTree b
unfold f a = case f a of
  Nothing         -> Leaf
  Just (a, b, a') -> Node (unfold f a) b (unfold f a')


-- exercise 2

treeBuild :: Integer -> BinaryTree Integer
treeBuild depth = unfold g (0, depth) -- the type a is (Int, Int) for "current depth" and "max depth"
  where g (d, md)
          | d >= md   = Nothing
          | otherwise = Just ((d+1,md), d, (d+1,md))
