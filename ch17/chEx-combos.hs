module Combinations where

import Control.Applicative (liftA3)

stops :: String
stops = "pbtdkg"

vowels :: String
vowels = "aeiou"

-- compare with chapter exercises 10.10, warmup problem #1
combos :: [a] -> [b] -> [c] -> [(a, b, c)]
combos as bs cs = liftA3 (,,) as bs cs

