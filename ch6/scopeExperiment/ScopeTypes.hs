module ScopeTypes where

class Sized a where
  size :: a -> Int

newtype ExactSize = ES Int

