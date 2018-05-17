module LanguageExercises where

import Data.Char

-- exercise 1
capitalizeWord :: String -> String
capitalizeWord (c:cs) = (toUpper c):cs


-- exercise 2
data ParseState = BeginString | LastWasPeriod | LeaveAlone
capitalizeParagraph :: String -> String
capitalizeParagraph s = go BeginString s
  where
    go _ [] = []
    go BeginString (c:cs) = (toUpper c):(go LeaveAlone cs)
    go LastWasPeriod (' ':cs) = (' '):(go LastWasPeriod cs)
    go LastWasPeriod (c:cs) = (toUpper c):(go LeaveAlone cs)
    go LeaveAlone ('.':cs) = ('.'):(go LastWasPeriod cs)
    go LeaveAlone (c:cs) = (c):(go LeaveAlone cs)
