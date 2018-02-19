module ChExGtWf where

i1 :: a -> a
i1 = id

c2 :: a -> b -> a
c2 a b = a

c3 :: b -> a -> b
c3 a b = a

c4 :: a -> b -> b
c4 a b = b

r5 :: [a] -> [a]
r5 = id
r5' as = as ++ as
r5'' as = [head as]
r5''' as = []

co6 :: (b -> c) -> (a -> b) -> a -> c
co6 bc ab a = bc $ ab a

a7 :: (a -> c) -> a -> a
a7 ac = id

a8 :: (a -> b) -> a -> b
a8 ab a = ab a

