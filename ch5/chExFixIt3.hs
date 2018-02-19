module Arith3Broken where

main :: IO ()
main = do -- main was capitalized
  print (1 + 2) -- needed parens
  putStrLn "10" -- putStrLn expects a String
  print (negate (-1::Int)) -- interesting! see bottom
  print ((+) 0 blah)
  where blah = negate 1

-- the (negate -1) line is notably interesting
--   :t negate :: Num a => a -> a
--   :t negate -1 :: (Num a, Num (a -> a)) => a -> a
--   :t negate (-1) :: Num a => a -- this is what I guessed the type would be
--   :t (negate -) 1 :: (Num a, Num (a -> a)) => a -> a -- same as (negate -1)
