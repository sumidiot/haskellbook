{-# LANGUAGE FlexibleInstances #-}
module FunctorInstances where


-- exercise 1
data Quant a b = Finance | Desk a | Bloor b
instance Functor (Quant a) where
  fmap _ Finance = Finance
  fmap _ (Desk a) = Desk a
  fmap f (Bloor b) = Bloor $ f b


-- exercise 2
data K a b = K a
instance Functor (K a) where
  fmap _ (K a) = K a -- ha, fascinating, fmap _ ka@(K a) = ka doesn't work!


-- exercise 3
newtype Flip f a b = Flip (f b a) deriving (Eq, Show)
newtype K' a b = K' a
instance Functor (Flip K' a) where
  fmap f (Flip (K' b)) = Flip (K' $ f b)


-- exercise 4
data EvilGoateeConst a b = GoatyConst b
instance Functor (EvilGoateeConst a) where
  fmap f (GoatyConst b) = GoatyConst $ f b


-- exercise 5
data LiftItOut f a = LiftItOut (f a)
instance Functor f => Functor (LiftItOut f) where
  fmap g (LiftItOut fa) = LiftItOut (fmap g fa)


-- exercise 6
data Parappa f g a = DaWrappa (f a) (g a)
instance (Functor f, Functor g) => Functor (Parappa f g) where
  fmap h (DaWrappa fa ga) = DaWrappa (fmap h fa) (fmap h ga)


-- exercise 7
data IgnoreOne f g a b = IgnoringSomething (f a) (g b)
instance Functor g => Functor (IgnoreOne f g a) where
  fmap h (IgnoringSomething fa gb) = IgnoringSomething fa (fmap h gb)


-- exercise 8
data Notorious g o a t = Notorious (g o) (g a) (g t)
instance Functor g => Functor (Notorious g o a) where
  fmap h (Notorious go ga gt) = Notorious go ga (fmap h gt)


-- exercise 9
data List a = Nil | Cons a (List a)
instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a as) = Cons (f a) (fmap f as)


-- exercise 10
data GoatLord a = NoGoat | OneGoat a | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)
instance Functor GoatLord where
  fmap f NoGoat = NoGoat
  fmap f (OneGoat a) = OneGoat (f a)
  fmap f (MoreGoats a a' a'') = MoreGoats (fmap f a) (fmap f a') (fmap f a'')


-- exercise 11
data TalkToMe a = Halt | Print String a | Read (String -> a)
instance Functor TalkToMe where
  fmap _ Halt = Halt
  fmap f (Print s a) = Print s (f a)
  fmap f (Read sa)   = Read (fmap f sa)

