-- This module demonstrates thinking of IO a as "a way to get an a, with IO side-effects"

module IoString where

-- ioString is a way to get a String, it just happens to have a side-effect
ioString :: IO String
ioString = do
  putStrLn "yo"
  return "dude"

main :: IO ()
main = do
  s <- ioString -- pull the string out of the result of ioString
  putStrLn s    -- and print it

