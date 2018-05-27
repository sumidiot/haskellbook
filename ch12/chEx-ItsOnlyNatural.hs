module ItsOnlyNatural where

data Nat =
  Zero | Succ Nat
  deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
netToInteger (Succ n) = 1 + natToInteger n

integerToNat :: Integer -> Maybe Nat
integerToNat i
  | i < 0     = Nothing
  | i == 0    = Just Zero
  | otherwise = fmap Succ (integerToNat (i-1))
