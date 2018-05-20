module StringProcessing where

import Data.Char -- for isAlpha

-- exercise 1

notThe :: String -> Maybe String
notThe "the" = Nothing
notThe s     = Just s

replaceThe :: String -> String
replaceThe s = go "" s ""
  where
    go w "" r = r ++ (checkWord w) -- try this with as-pattern for s@(c:cs), empty s as first guard, it fails!1
    go w (c:cs) r
      | isAlpha c = go (w++[c]) cs r
      | otherwise = go "" cs (r ++ (checkWord w) ++ [c])
    checkWord "the" = "a"
    checkWord s = s
