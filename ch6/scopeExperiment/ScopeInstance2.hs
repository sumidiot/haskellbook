module ScopeInstance2 where

import ScopeTypes

instance Sized ExactSize where
--  size :: ExactSize -> Int
  size (ES i) = 2 * i
