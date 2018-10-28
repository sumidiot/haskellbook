module FizzBuzzBasic where

import Data.Maybe (fromMaybe)

fizzBuzz :: Integer -> String
fizzBuzz n
  | n `mod` 15 == 0 = "FizzBuzz"
  | n `mod` 5  == 0 = "Buzz"
  | n `mod` 3  == 0 = "Fizz"
  | otherwise       = show n

main :: IO ()
main = mapM_ (putStrLn . fizzBuzz) [1..100]


-- nick's extension
-- function takes a list of (Int, String)
-- prints concatenation of the strings for all the ints its a multiple of

-- really, we're applying a bunch of functions that return Maybe M
gatherMatches :: Monoid m => [n -> Maybe m] -> n -> Maybe m
gatherMatches fs n =
  -- we have [f n] :: [Maybe m], given by fs <*> [n]
  -- given [t m], we want t [m], which is basically sequence
  -- however, for Maybe, that is an "All", only Some if all are Some
  -- i want "the other" one... "Any", Some of the ones that are Some
  -- sequence then gives us a Maybe [m] (although quits early, not what we want)
  -- and so we push mconcat through the Maybe to apply to the inner [m]
  --
  -- ha! the Monoid of Maybe m where m is a Monoid does this!
  mconcat $ fs <*> [n]

convertDivName :: (Integer, String) -> Integer -> Maybe String
convertDivName (d, s) n
  | n `mod` d == 0 = Just s
  | otherwise      = Nothing


funBuzz :: [(Integer, String)] -> Integer -> String
funBuzz divNames n =
  fromMaybe (show n) (gatherMatches (convertDivName <$> divNames) n)
                       
