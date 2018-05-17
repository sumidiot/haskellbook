module Phone where

-- valid buttons = "1234567890*#"
-- how do you write a good validating constructor? (Smart Constructors!)
type Digit = Char

-- valid presses: 1 and up
type Presses = Int

-- what does it mean to specify the type of DaPhone?
type DaPhone = Char -> [(Digit, Presses)]

convo :: [String]
convo =
  ["Wanna play 20 questions",
   "Ya",
   "U 1st haha",
   "Lol ok. Have u ever tasted alcohol",
   "Lol ya",
   "Wow ur cool haha. Ur turn",
   "Ok. Do you think I am pretty Lol",
   "Lol ya",
   "Just making sure rofl ur turn"]

-- given a Char, figure out how many Presses on which Digit
reverseTaps :: DaPhone -> Char -> [(Digit, Presses)]
reverseTaps = id
-- default phone definition would yield
-- 'a' -> [('2', 1)]
-- 'A' -> [('*', 1), ('2', 1)]

-- exercise #3
fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps l = sum $ map snd l

-- exercise #4
-- the book asks two questions: what is the most popular letter, and what is its cost?
-- that suggests the function returns a tuple
-- also, following reverseTaps, this function should take a DaPhone
mostPopularLetter :: DaPhone -> String -> (Char, Presses)
mostPopularLetter p s = (c, fingerTaps (reverseTaps p c))
  where c = mostOccurs s

data LMap k v = Empty | LMap (k, v) (LMap k v)
type CountMap k = LMap k Int

increment :: Eq k => (CountMap k) -> k -> (CountMap k)
increment Empty k = LMap (k, 1) Empty
increment (LMap (k, c) m) k'
  | k == k' = LMap (k, c+1) m
  | otherwise = LMap (k, c) (increment m k')

mapMax :: CountMap k -> (k, Int)
mapMax Empty = undefined
mapMax (LMap (k, v) m)
  | m <- Empty = (k, v)
  | v >= v'   = (k, v)
  | otherwise = (k', v')
  where (k', v') = mapMax m

tabulate :: Eq a => [a] -> CountMap a
tabulate [] = Empty
tabulate (a:as) = increment (tabulate as) a

mostOccurs :: Eq a => [a] -> a
mostOccurs = fst . mapMax . tabulate

-- exercise #5
coolestLtr :: [String] -> Char
coolestLtr = mostOccurs . concat

coolestWord :: [String] -> String
coolestWord = mostOccurs . concat . (map words)
