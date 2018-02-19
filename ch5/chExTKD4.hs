module TypeKwonDo4 where

munge :: (x -> y)
      -> (y -> (w, z))
      -> x
      -> w
munge xy ywz x = fst $ ywz $ xy x

