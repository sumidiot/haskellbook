module StringProcessing where

import Data.Char -- for isAlpha

-- exercise 1

notThe :: String -> Maybe String
notThe "the" = Nothing
notThe s     = Just s

replaceThe :: String -> String
replaceThe s = go "" s ""
  where
    go w "" r = r ++ (checkWord w) -- try this with as-pattern for s@(c:cs), empty s as first guard, it fails!
    go w (c:cs) r
      | isAlpha c = go (w++[c]) cs r
      | otherwise = go "" cs (r ++ (checkWord w) ++ [c])
    checkWord "the" = "a"
    checkWord s = s


-- exercise 2

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel s = go "" False s 0
  where 
    go _ _ "" r = r
    go _ True (c:cs) r
      | isVowel c = go "" False cs (r+1)
      where isVowel c = elem (toLower c) ['a', 'e', 'i', 'o', 'u']
    go w b (c:cs) r
      | isAlpha c = go (w++[c]) False cs r
      | otherwise = if w == "" then go "" b cs r else go "" (w == "the") cs r


-- execerise 3

countVowels :: String -> Integer
countVowels = foldr g 0
  where g c vs = vs + (if elem c ['a', 'e', 'i', 'o', 'u'] then 1 else 0)

