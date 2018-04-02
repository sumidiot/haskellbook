module ChapterExercises where

tensDigit :: Integral a => a -> a
tensDigit x = d
  where xLast = x `div` 10
        d     = xLast `mod` 10

tensDigit' :: Integral a => a -> a
tensDigit' x = dd
  where (d,m) = x `divMod` 10 -- not sure why we'd want to do this
        dd    = d `mod` 10

foldBool :: a -> a -> Bool -> a
foldBool l r b =
  case b of
    True -> l
    False -> r

foldBool' :: a -> a -> Bool -> a
foldBool' l r b
  | b = l
  | not b = r

roundTrip :: (Show a, Read a) => a -> a
roundTrip a = read (show a)

roundTrip' :: (Show a, Read a) => a -> a
roundTrip' = read . show

roundTrip'' :: (Show a, Read b) => a -> b
roundTrip'' = read . show

main = do
  print ((roundTrip'' 4) :: Int) -- oddly, :: String fails here, there's no instance Read String
  print (id 4)

