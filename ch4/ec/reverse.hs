-- module reverse.hs
module ReverseExtraCredit where

-- in this module, we aim to provide a function that
--   tokenizes a string (split on whitespace)
--   and returns the list of tokens in reverse order
-- the actual exercise should probably join the resulting list with strings,
--   as pointed out by attendees after the meetup for these chapters,
--   so the final function is String -> String
--   but I was more entertained by the `tokenize` task

-- tokenized reversal, split a string on spaces and reverse the list of tokens
tr :: String -> [String]
tr = reverse . tokenize
-- it's fun to write a function definition without even naming the argument


-- tokenize a string, split on spaces
-- tokenize :: String -> [String] -- tokenize is a thing of type (String -> [String])
-- we played, in the meetup, with how to read lines like this. `::` reads "is a"
-- but if you don't have the arrow until after the first argument, you have to read ahead
-- to know the thing is a function. putting the arrow up front means you can read it
-- as a sentence, "tokenize is a function String (to) [String]"
tokenize :: (->) String [String]
tokenize string = tokenize' [] "" string
  where -- this "hides" tokenize' from being exported in the module
    tokenize' :: [String] -> String -> String -> [String]
    -- the intuition is that you're walking through string, looking for spaces
    -- as you go, you've got
    --    complete words encountered so far
    --    a partial word you're looking for the end of (indicated by the next space)
    --    the rest of the string to process
    -- and then, when the rest of the string to process is empty, you finish the last word and return
    -- there's some edge cases we could put more thought into what we want
    --   e.g., multiple spaces, spaces at begin/end, etc
    tokenize' ws v (' ':s) = tokenize'  (ws++[v])  ""        s -- ws = "words", v = "part of a _w_ord"
    tokenize' ws v (c:s)   = tokenize'  ws         (v++[c])  s -- c = "character", s = "rest of string"
    tokenize' ws v _       = ws++[v] -- matches empty third argument
-- after the meetup, another attendee posted a solution that eliminates the first
-- argument of `tokenize'`, pushing it into the final return right away.
-- that's a nice solution. i _think_ the way above is tail recursive, where
-- the other solution isn't?
-- while formatting the above, i learned that you need spaces around... the places where there
-- are spaces above :)
