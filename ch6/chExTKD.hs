module ChExTypeKwonDo where

chk :: Eq b => (a -> b) -> a -> b -> Bool
chk ab a b = (ab a) == b
-- alternates: always return True, or always return False

arith :: Num b
      => (a -> b)
      -> Integer
      -> a
      -> b
arith ab _ a = ab a
-- alternate: fromInteger i could pull the second argument to type b, then you could + - *, e.g.,
