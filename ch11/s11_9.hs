{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

module S11_9 where

class TooMany a where
  tooMany :: a -> Bool


-- Exercise 1

--instance TooMany (Int, String) where
--  tooMany (i,s) = (length s) > i

-- that's what I tried first, but I get the "FlexibleInstances" the book talks about
-- reading the error message, it basically says the things you'd put in for `a`
-- have to be distinct type variables, and that's where the `newtype` suggestion comes in i guess

newtype ISPair = ISPair (Int, String)

instance TooMany ISPair where
  tooMany (ISPair (l,s)) = (length s) > l


-- Exercise 2
newtype Goats = Goats Int deriving (Eq, Show)
instance TooMany Goats where
  tooMany (Goats i) = i > 40

newtype IIPair = IIPair (Int, Int)
instance TooMany IIPair where
  tooMany (IIPair (i,j)) = tooMany (Goats (i+j))


-- Exercise 3
instance TooMany Int where
  tooMany i = i > 45
newtype Goats' = Goats' Int deriving (Eq, Show, Num, TooMany)
instance (Num a, TooMany a) => TooMany (a, a) where
  tooMany (n, t) = tooMany t

-- so what just happened?
-- I think the instance for TooMany (a, a) is set up as intended. It requires the FlexibleInstances pragma.
-- to test that instance, we need a type that is both TooMany and Num, so we created
-- Goat'. It picks up the Num instance automatically from the inner Int type, and then
-- we also added the instance TooMany Int so it could automatically get that instance too.
-- Now you can call tooMany (Goats' 3, Goats' 7)
