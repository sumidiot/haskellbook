module S10_4 where

showSumCall :: Int -> Int -> IO Int
showSumCall a b =
  do
    print $ concat [show a,"+",show b,"=",show (a+b)]
    return (a+b)

-- ok, fun problem. i have showSumCall :: Int -> Int -> IO Int
-- I want to fold over that on a [Int], but the types don't work
-- I can `map pure [1..5]` to produce a list of `IO Int` I can fold over,
-- but then I need my showSumCall to take IO Int -> IO Int -> IO Int
ugliness :: Int -> IO Int -> IO Int
ugliness a = (<*>) $ pure (a+)
-- that gets the type signature correct, but doesn't actually do any IO

-- can I pull one argument off at a time?
leftSumOne :: Int -> IO Int -> IO Int
leftSumOne a b =
  do
    b' <- b
    print $ concat [show a,"+",show b',"=",show (a+b')]
    return (a+b')

leftConstOne :: Int -> IO Int -> IO Int
leftConstOne a b =
  do
    print $ concat [show a," <- const _ = ", show a]
    return (const a b)

-- ok, go back, really i want to see what happens, i can re-implement foldr
foldr' :: (a -> IO b -> IO b) -> IO b -> [a] -> IO b
foldr' f z [] = z
foldr' f z (a:as) =
  -- f a (foldr' f z as)
  do
    f a (foldr' f z as)

