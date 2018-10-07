module FilterF where

-- Applicative f gives
--   pure :: a -> f a
--   <*> :: f (a -> b) -> f a -> f b
--    *> :: f a -> f b -> f b
--   <*  :: f a -> f b -> f b
-- Monoid (f a) gives
--   mempty :: f a
--   <> :: f a -> f a -> f a

filterF :: (Applicative f, Foldable t, Monoid (f a)) => (a -> Bool) -> t a -> f a
filterF p as = foldMap (\a -> if p a then pure a else mempty) as

