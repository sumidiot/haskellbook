module MonoidExercises where

-- skipping to exercise 8, because it looks interesting

import Data.Monoid


-- exercise 8
newtype Mem s a = Mem { runMem :: s -> (a, s) }

instance Monoid a => Monoid (Mem s a) where
  mempty = Mem (\s -> (mempty, s))
  mappend (Mem f) (Mem g) = Mem h
                            where h s = -- first `let` i've actually sorta _needed_
                                        let (a, s') = g s
                                            (a', s'') = f s'
                                        in (a <> a', s'')


f' = Mem $ \s -> ("hi", s + 1)
main = do
  let rmzero = runMem mempty 0
      rmleft = runMem (f' <> mempty) 0
      rmright = runMem (mempty <> f') 0
  print $ rmleft
  print $ rmright
  print $ (rmzero :: (String, Int))
  print $ rmleft == runMem f' 0
  print $ rmright == runMem f' 0
  -- why not show `print $ runMem (f' <> f') 0`


