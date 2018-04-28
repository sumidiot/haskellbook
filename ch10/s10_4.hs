module S10_4 where

showSumCall :: Int -> Int -> IO Int
showSumCall a b =
  do
    print $ concat [show a,"+",show b,"=",show (a+b)]
    return (a+b)

showSumCall' :: IO Int -> IO Int -> IO Int
showSumCall' ioA ioB =
  do
    a <- ioA
    b <- ioB
    print $ concat [show a,"+",show b,"=",show (a+b)]
    return (a+b)

ssc = foldr showSumCall' (pure 0) (map pure [1..5])

-- now try foldr showSumCall' (pure 0) (map pure [1..5])
-- you do see that "5+0=5" gets computed first, but I still feel like that's more of
-- artifact of how 1 + (2 + (3 + (4 + (5 + 0)))) is computed for the 'top level' + after the 1
-- e.g., if we showConstCall we'd just see 1 <-const-

showConstCall :: IO Int -> IO Int -> IO Int
showConstCall ioA ioB =
  do
    print "getting a"
    a <- ioA
    print "getting b"
    b <- ioB
    print $ concat [show a," <-const- ",show b,"=",show (const a b)]
    return (const a b)

scc = foldr showConstCall (pure 0) (map pure [1..5])

-- the first apparent const call here is the `5 <-const- 0` call, but somewhat that's because
-- we artificially forced the computation of b so we could print it

countFoldCallsConst :: Int -> (Int,Int) -> (Int,Int)
countFoldCallsConst r (c,r') = (1+c,const r r')

cfcc = foldr countFoldCallsConst (0,0) [1..5]

-- why is this (5,1)?

-- foldr f z xs =
--   case xs of
--     []      -> z
--     (x:xs)  -> f x (foldr f z xs)
--
-- foldr const 0 [1,2]
--   = const 0 (foldr const 0 [2])
--   = 0
--

myConst :: a -> b -> a
myConst a _ = a

countFoldCallsMyConst :: Int -> (Int,Int) -> (Int,Int)
countFoldCallsMyConst r (c,r') = (1+c, myConst r r')

cfcmc = foldr countFoldCallsMyConst (0,0) [1..5]
