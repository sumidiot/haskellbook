module S9_6 where -- Extracting portions of lists

-- exercise 1, split a string on spaces
myWords :: String -> [String]
myWords "" = []
myWords s = (takeWhile (/= ' ') (dropWhile (== ' ') s)) : myWords popToken
  where
    popToken = dropWhile (== ' ') (dropWhile (/= ' ') s)

-- exercise 2, split a string on newlines
myLines :: String -> [String]
myLines "" = []
myLines s = takeToken : myLines dropToken
  where
    takeToken = takeWhile (/= '\n') (dropWhile (== '\n') s)
    dropToken = dropWhile (== '\n') (dropWhile (/= '\n') s)

firstSen = "Tyger Tyger, burning bright\n"
secondSen = "In the forests of the night\n"
thirdSen = "What immortal hand or eye\n"
fourthSen = "Could frame they fearful symmetry?"
sentences = firstSen ++ secondSen ++ thirdSen ++ fourthSen

-- how do you print a list of strings?
-- mapM_ found at: https://stackoverflow.com/questions/5289779/printing-elements-of-a-list-on-new-lines
demo2 :: IO ()
demo2 = do
  putStrLn "Exercise 2:"
  putStrLn sentences
  mapM_ putStrLn (myLines sentences)


-- exercise 3
splitStringByChar :: Char -> String -> [String]
splitStringByChar _ "" = []
splitStringByChar c s = takeToken : splitStringByChar c dropToken
  where
    takeToken = takeWhile (/= c) (dropWhile (== c) s)
    dropToken = dropWhile (== c) (dropWhile (/= c) s)

-- this version provides flexibility to, for example, split on a list of characters
splitStringByCharPredicate :: (Char -> Bool) -> String -> [String]
splitStringByCharPredicate _ "" = []
splitStringByCharPredicate p s = takeToken : splitStringByCharPredicate p dropToken
  where
    takeToken = takeWhile (not . p) (dropWhile p s)
    dropToken = dropWhile p (dropWhile (not . p) s)

spaceOrPipe :: Char -> Bool
spaceOrPipe ' ' = True
spaceOrPipe '|' = True
spaceOrPipe  _  = False

