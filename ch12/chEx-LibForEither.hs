module LibForEither where

-- exercise 1

lefts' :: [Either a b] -> [a]
lefts' = foldr g []
  where g (Right b) r = r
        g (Left b)  r = b:r


-- exercise 2

rights' :: [Either a b] -> [b]
rights' = foldr g []
  where g (Right b) r = b:r
        g (Left b)  r = r


-- exercise 3

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' = foldr g ([], [])
  where g (Left a) (ra, rb) = (a:ra, rb)
        g (Right b) (ra, rb) = (ra, b:rb)


-- exercise 4

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' _ (Left _) = Nothing
eitherMaybe' f (Right b) = Just (f b)


-- exercise 5

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f _ (Left a) = f a
either' _ g (Right b) = g b


-- exercise 6

eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' f = either' (const Nothing) (Just . f)
