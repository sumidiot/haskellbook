-- Mood Swing exercises, 4.3
module MoodSwing where

data Mood = Blah | Woot deriving Show

changeMood :: Mood -> Mood
changeMood Blah = Woot
changeMood    _ = Blah -- or replace _ with Woot
