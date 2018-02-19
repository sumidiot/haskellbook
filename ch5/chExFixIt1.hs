module Sing where -- module name needs to be capitalized

fstString :: [Char] -> [Char] -- typo, ++ should be ->
fstString x = x ++ " in the rain"

sndString :: [Char] -> [Char] -- output is String, not Char
sndString x = x ++ " over the rainbow"

sing :: String -- didn't include type declaration
sing = if (x > y) then fstString x else sndString y
  where x = "Singin"
        y = "Somewhere"
