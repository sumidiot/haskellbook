-- print3.hs
module Print3 where

myGreeting :: String
myGreeting = "hello" ++ " world!"

hello :: String
hello = "hello"

world :: String
world = "world!"

main :: IO ()
main = do
  putStrLn myGreeting
  putStrLn secondGreeting
     where secondGreeting = -- next line must be indented more than the start of 'secondGreeting'
             concat [hello, " ", world]

