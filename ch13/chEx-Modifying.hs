module ChExModifying where

import Control.Monad -- exercise 2
import System.Exit -- exercise 2
import Data.Char -- exercise 3


-- exercise 1
-- actually done in chEx-Vigenere


-- exercise 2
palindrome :: IO ()
palindrome = forever $ do
  line1 <- getLine
  case (line1 == reverse line1) of
    True -> putStrLn "It's a palindrome!"
    False -> do putStrLn "Nope!"
                exitSuccess


-- exercise 3
palindrome' :: String -> Bool
palindrome' s = (ess == reverse ess)
  where ess = map toLower $ filter isAlpha s


-- exercise 4
type Name = String
type Age = Integer

data Person = Person Name Age deriving Show

data PersonInvalid =
  NameEmpty | AgeTooLow | PersonInvalidUnknown String
  deriving (Eq, Show)

mkPerson :: Name -> Age -> Either PersonInvalid Person
mkPerson name age
  | name /= "" && age > 0 = Right $ Person name age
  | name == "" = Left NameEmpty
  | not (age > 0) = Left AgeTooLow
  | otherwise =
      Left $ PersonInvalidUnknown $
        "Name was: " ++ show name ++ " Age was: " ++ show age

gimmePerson :: IO ()
gimmePerson = do
  putStr "Enter a name: "
  name <- getLine
  putStr "Enter an age: "
  ageString <- getLine
  putStrLn $ case mkPerson name (read ageString) of
    Left NameEmpty -> "An error occurred: name was empty"
    Left AgeTooLow -> "An error occurred: age was too low"
    Left (PersonInvalidUnknown e) -> "An error occurred: " ++ e
    Right p -> "Yay! Successfully got a person: " ++ show p
