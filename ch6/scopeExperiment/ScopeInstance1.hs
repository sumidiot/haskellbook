module ScopeInstance1 where

import ScopeTypes

instance Sized ExactSize where
--  size :: ExactSize -> Int
  size (ES i) = i
