module ModuleHelpers where

-- exercise 1
j :: Monad m => m (m a) -> m a
j mma = mma >>= id


-- exercise 2
l1 :: Monad m => (a -> b) -> m a -> m b
l1 = fmap


-- exercise 3
l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
-- fmap f ma :: m (b -> c), and then i've got an m b, so i just wanna slam those together...
l2 f ma mb = (fmap f ma) <*> mb
-- aka liftM2 (from Control.Monad)


-- exercise 4
a :: Monad m => m a -> m (a -> b) -> m b
a = flip (<*>)


-- exercise 5
meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh [] _ = return []
meh (a:as) f = (l2 (:)) (f a) (meh as f)
-- so if i apply f to the head, a, i guess (f a) of type m b
-- if i recurse on the tail, (meh as f) has type m [b]
-- so then, how do i combine an m b and an m [b]?
-- well, i want to lift (:), giving me an m b -> m [b] -> m [b]


-- exercise 6
flipType :: Monad m => [m a] -> m [a]
flipType mas = meh mas id
