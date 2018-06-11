module Puzzle where

import Control.Monad (forever)
import Data.Char (toUpper)
import Data.List (intersperse, sortBy)
import Data.Maybe (isJust)
import System.Exit (exitSuccess)

-- arguments are
--   word we're trying to guess
--   characters we've filled in so far
--   letters guessed so far
data Puzzle =
  Puzzle String [Maybe Char] [Char]


instance Show Puzzle where
  show (Puzzle _ discovered guessed) =
    (intersperse ' ' $ fmap renderPuzzleChar discovered)
    ++ " Guessed so far: " ++ guessed


freshPuzzle :: String -> Puzzle
freshPuzzle s = Puzzle ess empties []
  where empties = map (const Nothing) ess
        ess     = map toUpper s


renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar Nothing  = '_'
renderPuzzleChar (Just c) = c


charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle w _ _) c = elem (toUpper c) w

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ g) c = elem (toUpper c) g


fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle w s g) c = Puzzle w s' (addGuess g cee)
  where cee = toUpper c
        addGuess g cee
          | elem cee g = g
          | otherwise  = sortBy compare (cee:g)
        s' = zipWith (zipper cee) w s
        zipper c wc sc
          | c == wc   = Just c
          | otherwise = sc


handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess p g = do
  putStrLn $ "Your guess was: " ++ [g]
  case (charInWord p g, alreadyGuessed p g) of
    (_, True) -> do
      putStrLn "You already guessed that character, pick something else!"
      return p
    (True, _) -> do
      putStrLn "This character was in the word, filling in the word accordingly"
      return (fillInCharacter p g)
    (False, _) -> do
      putStrLn "This character wasn't in the word, try again."
      return (fillInCharacter p g)


-- this is the addition i added for the Chapter Exercise to normalize the game logic
-- we only want to penalize wrong guesses, which we can get as as
--   characters that are in the guess that aren't in the word
gameIsOver :: Puzzle -> Bool
gameIsOver (Puzzle w _ g) = numIncorrect > 7
  where numIncorrect = length $ filter (not . ((flip elem) w)) g


gameOver :: Puzzle -> IO ()
gameOver p@(Puzzle w _ g) =
  if gameIsOver p then
    do putStrLn "You lose!"
       putStrLn $ "The word was: " ++ w
       exitSuccess
  else
    return ()


gameWin :: Puzzle -> IO ()
gameWin (Puzzle _ filledInSoFar _) =
  if all isJust filledInSoFar then
    do putStrLn "You win!"
       exitSuccess
  else
    return ()


runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do
  gameOver puzzle
  gameWin puzzle
  putStrLn $ "Current puzzle is: " ++ show puzzle
  putStr "Guess a letter: "
  guess <- getLine
  case guess of
    [c] -> handleGuess puzzle c >>= runGame
    _   -> putStrLn "Your guess must be a single character"
