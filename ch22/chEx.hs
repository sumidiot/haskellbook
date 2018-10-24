module ChapterExercise where -- the book calls the module ReaderPractice

import Control.Applicative
import Data.Maybe

x = [1, 2, 3]
y = [4, 5, 6]
z = [7, 8, 9]

-- lookup is in the Prelude
-- lookup :: Eq a => a -> [(a, b)] -> Maybe b

-- zip x and y using 3 as the lookup key
xs :: Maybe Integer
xs = lookup 3 $ zip x y

-- zip y and z using 6 as the lookup key
ys :: Maybe Integer
ys = lookup 6 $ zip y z

-- this one should be Nothing
-- zip x and y using 4 as the lookup key
zs :: Maybe Integer
zs = lookup 4 $ zip x y

z' :: Integer -> Maybe Integer
z' n = lookup n $ zip x y


-- make a Maybe (,) of values using Applicative
x1 :: Maybe (Integer, Integer)
x1 = liftA2 (,) xs ys

x2 :: Maybe (Integer, Integer)
x2 = liftA2 (,) ys zs

-- apply z' twice, diag z'
x3 :: Integer -> (Maybe Integer, Maybe Integer)
x3 = liftA2 (,) z' z'
-- this is surprising somehow. we went from lifting specific Maybes to lifting functions which produce Maybe
-- it seems like the Maybe is just hiding things here, liftA2 (,) (+1) (+1)


-- uncurry :: (a -> b -> c) -> (a, b) -> c
summed :: Num c => (c, c) -> c
summed = uncurry (+)

-- i guess the idea is this is supposed to be "bigger or less than" 3 and 8, except "and"...
bolt :: Integer -> Bool
-- (>3) and (<8) are both Integer -> Bool
-- (&&) takes Bool -> Bool -> Bool
bolt = liftA2 (&&) (>3) (<8)

-- it's fun to lift a function of two arguments into the "context" of functions awaiting an input
-- i.e., the whole point of Reader

-- fromMaybe :: a -> Maybe a -> a

sequA :: Integral a => a -> [Bool]
sequA = sequenceA [(>3), (<8), even]

s' = summed <$> ((,) <$> xs <*> ys) -- Maybe sums of all pairs of xs and ys (i.e. Just 7)


main :: IO ()
main = do
  print $ sequenceA [Just 3, Just 2, Just 1] -- sequence flips structure, so this is a Just [ Int ]
  print $ sequenceA [x, y]
  print $ sequenceA [xs, ys]
  print $ summed <$> ((,) <$> xs <*> ys)
  print $ fmap summed ((,) <$> xs <*> zs)
  print $ bolt 7
  print $ fmap bolt z
  print $ sequenceA [(>3), (<8), even] 7
  print $ all id (sequA 7) -- or fold (&&) True (sequA 7) -- exercise 1
  print $ sequA (fromMaybe 7 s') -- exercise 2
  print $ bolt (fromMaybe 7 ys) -- exercise 3

