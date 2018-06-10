module Main where

-- any of these still needed?
import Control.Monad (forever)
import Data.Maybe (isJust)
import System.Exit (exitSuccess)

import Hangman
import Puzzle

main :: IO ()
main = do
  word <- randomWord'
  runGame $ freshPuzzle word
