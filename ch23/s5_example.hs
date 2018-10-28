module RandomExample where

import System.Random
import Control.Applicative (liftA3)
import Control.Monad (replicateM)
import Control.Monad.Trans.State

data Die = DieOne | DieTwo | DieThree | DieFour | DieFive | DieSix deriving (Eq, Show)

intToDie :: Int -> Die
intToDie n = case n of
  1 -> DieOne
  2 -> DieTwo
  3 -> DieThree
  4 -> DieFour
  5 -> DieFive
  6 -> DieSix
  x -> error $ "intToDie got non 1-6 integer: " ++ show x -- don't use `error`, this is for example only

-- not optimal, but usable
rollDieThreeTimes :: (Die, Die, Die)
rollDieThreeTimes = do
  let s = mkStdGen 0
      (d1, s1) = randomR (1, 6) s
      (d2, s2) = randomR (1, 6) s1
      (d3, _)  = randomR (1, 6) s2
  (intToDie d1, intToDie d2, intToDie d3)


-- version 2

rollDie :: State StdGen Die -- StdGen -> (Die, StdGen)
rollDie = state $ do
  (n, s) <- randomR (1, 6)
  return (intToDie n, s)

rollDie' :: State StdGen Die
rollDie' = intToDie <$> state (randomR (1, 6)) -- state (randomR (1, 6)) isA StateT s m Int
                                               -- where probably m is Identity, s is RandomGen

rollDieThreeTimes' :: State StdGen (Die, Die, Die)
rollDieThreeTimes' = liftA3 (,,) rollDie rollDie rollDie


nDie :: Int -> State StdGen [Die]
nDie n = replicateM n rollDie
-- evalState (nDie 5) (mkStdGen 0)


-- roll until a sum is greater than 20
rollsToGetTwenty :: StdGen -> Int
rollsToGetTwenty g = go 0 0 g
  where
    go :: Int -> Int -> StdGen -> Int
    go sum count gen
      | sum >= 20 = count
      | otherwise =
        let (die, nextGen) = randomR (1, 6) gen
        in go (sum + die) (count + 1) nextGen


-- Roll Your Own exercises

-- exercise 1
rollsToGetN :: Int -> StdGen -> Int
rollsToGetN b g = go 0 0 b g
  where
    go :: Int -> Int -> Int -> StdGen -> Int
    go sum count bound gen
      | sum >= bound = count
      | otherwise =
        let (die, nextGen) = randomR (1, 6) gen
        in go (sum + die) (count + 1) bound nextGen

-- exercise 2
-- note we can drop the inner 'count' because it's the length of the [Die]
rollsCountLogged :: Int -> StdGen -> (Int, [Die])
rollsCountLogged b g = go 0 [] b g
  where
    go :: Int -> [Die] -> Int -> StdGen -> (Int, [Die])
    go sum rolls bound gen
      | sum >= bound = (length rolls, rolls)
      | otherwise =
        let (die, nextGen) = randomR (1, 6) gen
        in go (sum + die) ((intToDie die) : rolls) bound nextGen

