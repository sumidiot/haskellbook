module Main where

import Hangman
import Puzzle

main :: IO ()
main = do
  word <- randomWord'
  runGame $ freshPuzzle word
