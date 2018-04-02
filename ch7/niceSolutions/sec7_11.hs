{- 7.11 Chapter exercises -}

-- Multiple Choice
-- ---------------

-- 1. a polymorphic function d)
--    may resolve to values of different types
--
-- 2. Given two functions
-- 	f :: Char -> String
-- 	g :: String -> [String]
--   The composed g . f has the type
--   a) Char -> String
--
-- 3. Given f :: Ord a => a -> a -> Bool
--    and we apply it to one numeric value,
--    the type is now
--    d) (Ord a, Num a) => a -> Bool
--
-- 4. A function with the type (a -> b) -> c
--    b) is a higher order function.
--
-- 5. Given 
-- 	f :: a -> a
-- 	f x = x
--    Then the type of (f True)
--    is (f True) :: Bool.
--

-- tests
-- 2.
f2 :: Char -> String
f2 x = [x]

g2 :: String -> [String]
g2 x = [x]

z2 = g2 . f2

-- 5.
f5 :: a -> a
f5 x = x


-- Let's Write Code
-- ----------------

-- 1. the ten digit of an Integral argument
-- Original:
tensDigit :: Integral a => a -> a
tensDigit x = d
  where xLast = x `div` 10
        d = xLast `mod` 10

--    a) rewrite with divMod
tensDigitA x = d 
  where 
   divMod10 = flip divMod 10
   l = (fst . divMod10) x
   d = (snd . divMod10) l -- expanded fold
 
--    test with: map tensDigitA [0..100]

--   b) same type?
--      Yes.

--   c) Hundreds digit.

hunsD x = toDigit x 3
  where
   toDigit x n = case n of -- recursive impl.
     1 -> x `mod` 10
     otherwise -> toDigit (x `div` 10) (n - 1)

-- test over some digits:
--   let s = [x | x <- [0..300], x `mod` 4 == 0] in
--     map (\x -> (x, hunsD x)) s

-- 2. implement the following using case and guard:
-- Original from book:
foldBool3 :: a -> a -> Bool -> a
foldBool3 x _ False = x
foldBool3 _ y True = y

foldBoolCase x y z = case z of
  True -> y
  otherwise -> x

foldBoolGuard x y z 
  | z = y
  | otherwise = x

-- 3. Fill in definition
g :: (a->b)->(a,c)->(b,c)
g f (a,c) = (f a, c)

-- To test:  g show (1, 2)
-- Expected: ("1", 2)

-- 4. Run this from book:
roundTrip :: (Show a, Read a) => a -> a 
roundTrip a = read (show a)
runRoundTrip = do
  print (roundTrip 4) 
  print (id 4)

-- 5. Make point free version of 4.
roundTrip2 :: (Show a, Read a) => a -> a
roundTrip2 = read . show
-- ^-- not actually equivalent to `id`

-- 6. Make (Show a, Read b) => a -> b
roundTrip3 :: (Show a, Read b) => a -> b
roundTrip3 = read . show
-- use like this:
--    (roundTrip3 5) :: Integer
