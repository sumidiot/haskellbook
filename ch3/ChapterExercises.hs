module ChapterExercises where

-- Building functions, #2

exclaim :: String -> String
exclaim s = s ++ "!"

fifthChar :: [Char] -> Char
fifthChar s = s !! 4

backend :: [Char] -> [Char]
-- backend s = drop 9 s
backend = drop 9

-- Building functions, #3
thirdLetter :: String -> Char
thirdLetter s = s !! 2

-- Building functions, #4
nthLetter :: Int -> Char
nthLetter n = s !! n
  where s = "Curry is awesome!"

-- Building functions, #5
rvrs = a ++ i ++ c
  where s = "Curry is awesome"
        a = backend s -- exercise #2
        i = take 4 (drop 5 s)
        c = take 5 s

