module SpecTypes where


-- exercise 1
-- for the type []
pureForList :: a -> [] a -- or a -> [a]
appForList  :: [a -> b] -> [a] -> [b]


-- exercise 2
-- for the type IO
pureForIO :: a -> IO a
appforIO  :: IO (a -> b) -> IO a -> IO b


-- exercise 3
-- for the type (,) a
pureForTuple :: a -> (a, a) -- do they really mean a for the (,) a type?
appForTuple  :: (a, a -> b) -> (a, a) -> (a, b)


-- exercise 4
-- for the type (->) e
pureForFunc :: a -> (e -> a)
appForFunc  :: (e -> a -> b) -> (e -> a) -> (e -> b)
