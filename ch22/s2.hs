module Section2 where

import Control.Applicative

boop :: Num t => t -> t
boop = (*2)

doop :: Num t => t -> t
doop = (+10)

bip :: Num t => t -> t
bip = boop . doop

bloop :: Integer -> Integer
bloop = fmap boop doop
-- the "context" (functor) that `boop` is being lifted over is the partially-applied `doop`


-- here's where we start using Applicative
-- takes one argument, sends to both boop and doop, and then adds the results
bbop :: Integer -> Integer
bbop = (+) <$> boop <*> doop

-- same as bbop
duwop :: Integer -> Integer
duwop = liftA2 (+) boop doop


boopDoop :: Integer -> Integer
boopDoop = do
  a <- boop
  b <- doop
  return (a + b)

bloop' :: Integer -> Integer
bloop' = do
  a <- doop
  return (boop a)

