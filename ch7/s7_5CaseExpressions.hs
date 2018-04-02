module Section7_5 where

funcZ x =
  case x + 1 == 1 of
    True -> "AWESOME"
    False -> "wut"

pal xs =
  case xs == reverse xs of
    True -> "yes"
    False -> "no"

pal' xs =
  case y of
    True -> "yes"
    False -> "no"
  where y = xs == reverse xs

greetIfCool :: String -> IO ()
greetIfCool coolness =
  case cool of
    True ->
      putStrLn "eyyy. What's shakin'?"
    False ->
      putStrLn "pshh"
  where cool =
          coolness == "downright frosty yo"

