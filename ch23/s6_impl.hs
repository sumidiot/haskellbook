{-# LANGUAGE InstanceSigs #-}

module Moi where

newtype Moi s a = Moi { runMoi :: s -> (a, s) }


instance Functor (Moi s) where
  -- we can write the specialized type signature here because of
  -- the LANGUAGE InstanceSigs pragma
  fmap :: (a -> b) -> Moi s a -> Moi s b
  fmap f (Moi g) = Moi $ \s -> let (a, s') = g s in
                                 (f a, s')


instance Applicative (Moi s) where
  pure :: a -> Moi s a
  pure a = Moi $ \s -> (a, s)

  (<*>) :: Moi s (a -> b) -> Moi s a -> Moi s b
  (Moi f) <*> (Moi g) =
    -- we have f :: s -> (a -> b, s)
    --     and g :: s -> (a     , s)
    -- we want h :: s -> (     b, s)
    -- so, given s, we map it by f to get (ab, s'),
    --                 then apply g to s' to get (a, s'')
    --                 and return (ab a, s'')
    Moi $ \s -> let (ab, s') = f s
                    (a, s'') = g s'
                in
                    (ab a, s'')


instance Monad (Moi s) where
  return = pure

  (>>=) :: Moi s a -> (a -> Moi s b) -> Moi s b
  (Moi f) >>= g =
    -- we have
    --   f :: s -> (a, s)
    --   g :: a -> (s -> (b, s))
    -- we want
    --   h :: s -> (b, s)
    -- so, given s
    --   apply f to get (a, s')
    --   then apply g to a and s' to get (b, s'')
    Moi $ \s -> let (a, s') = f s
                    m = g a
                in
                    runMoi m s'
