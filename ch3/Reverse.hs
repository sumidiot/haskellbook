-- Chapter Exercise, Building functions 6

module Reverse where

rvrs :: String -> String
rvrs x = back ++ mid ++ front
  where back = drop 9 x
        mid = take 4 (drop 5 x)
        front = take 5 x

main :: IO ()
main = print $ rvrs "Curry is awesome"
