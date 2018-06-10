module Vigenere where

import Data.Char
-- :t chr Int -> Char
-- :t ord Char -> Int

-- pulled from chapter 9, re-arranged slightly
caesarEncC :: Int -> Char -> Char
caesarEncC n x = snd (head (filter (\t -> fst t == x) (cmap m)))
  where cmapi = ' ' : ['a'..'z'] ++ ['A'..'Z']
        cmapo = ' ' : rot m ['a'..'z'] ++ rot m ['A'..'Z']
        rot n xs = (drop n xs) ++ (take n xs)
        cmap n = zip cmapi cmapo
        m = n `mod` 26 -- handles negatives

-- what would it look like if we assume the char is an uppercase alphabetic character?
caesarEncC' :: Int -> Char -> Char
caesarEncC' n x = chr (((((ord x) - 65) + n) `mod` 26) + 65)

caesarEncC'' :: Int -> Char -> Char
caesarEncC'' n x
  | 'A' <= x && x <= 'Z' = rot n 65 x
  | 'a' <= x && x <= 'z' = rot n 97 x
  | otherwise            = x
  where rot n s c = chr (((((ord c) - s) + n) `mod` 26) + s)

-- to compare timings of caesarEncC and caesarEncC'
--   let longString = map (\i -> chr (65 + (i `mod` 26))) [0..1000000]
--   :set +s
--   last $ map (caesarEnc 9) longString
--   last $ map (caesarEnc'' 9) longString
-- note you have to force the map, simply `let a = map f l` doesn't seem to do any work

caesarEncS :: Int -> String -> String
caesarEncS n s = map (caesarEncC n) s

caesarDecS :: Int -> String -> String
caesarDecS n e = caesarEncS (-n) e

-- so, this does something reasonably correct, but different from the book example
-- in particular, this version applies the cipher to all characters (e.g., spaces)
-- where the book only applies it to alphabetic characters.
vigenereEnc :: String -> String -> String
vigenereEnc key s = map (uncurry caesarEncC) (zip rots s)
  where rots = map (asInt . inKey) [0..]
        inKey i = key !! (i `mod` (length key))
        asInt c = (ord (toUpper c)) - 65

--e c (n,l)
--    | ('a' <= c && c <= 'z') = (n + 1, l ++ [e' n c])
--    | otherwise              = (n, l ++ [c])
--    where e' = caesarEncC''

-- it seems like the version that only applies the cipher to alphabetic characters,
-- and thus only sometimes increments in the key, is a little bit harder
vigenereEnc' :: String -> String -> String
vigenereEnc' key s = snd $ foldl enc (0,[]) s -- foldl, because foldr goes backwards
  where enc (curShift, encSoFar) c
            | shiftingChar c = (nextShift curShift, encSoFar ++ [enc' curShift c])
            | otherwise      = (curShift          , encSoFar ++ [c])
        shiftingChar c = ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z')
        nextShift n    = (n + 1) `mod` (length key)
        enc' n c       = caesarEncC'' (charAsRot (key !! n)) c
        charAsRot c    = (ord (toUpper c)) - 65


encodeCaesar :: IO ()
encodeCaesar = do
  putStr "Enter the string to encode: "
  plaintext <- getLine
  putStr "Enter the shift: "
  shiftStr <- getLine
  putStrLn (caesarEncS (read shiftStr) plaintext)


encodeVigenere :: IO ()
encodeVigenere = do
  putStr "Enter the string to encode: "
  plaintext <- getLine
  putStr "Enter the passphrase: "
  passphrase <- getLine
  putStrLn (vigenereEnc' passphrase plaintext)


