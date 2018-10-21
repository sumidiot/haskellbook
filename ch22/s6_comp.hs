module ReadingComprehension where

newtype Reader r a = Reader { runReader :: r -> a }

-- exercise 1
myLiftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
-- f applicative means we have <*> :: f (x -> y) -> f x -> f y
--    as well as functor       <$> ::   (x -> y) -> f x -> f y
-- take x = a, y = (b -> c),   <$> :: (a -> (b -> c)) -> f a -> f (b -> c)
--    so (abc <$> fa) is a f (b -> c)
--    and we have an fb, so f is still applicative, so we <*> it
myLiftA2 abc fa fb = abc <$> fa <*> fb
-- i'm not sure if i cheated here, or what


-- exercise 2
asks :: (r -> a) -> Reader r a
asks f = Reader f

instance Functor (Reader r) where
  fmap f (Reader ra) = Reader $ f . ra

instance Applicative (Reader r) where
  pure = Reader . const
  (Reader rab) <*> (Reader ra) = Reader $ \r -> rab r (ra r)

-- i'd love to have the checkers check on this, but i'm not entirely sure how to get there
-- the Reader type is a bit more complicated than the other datatypes we've used, because
-- of the function in there... Eq and Show are going to have issues, I think.



