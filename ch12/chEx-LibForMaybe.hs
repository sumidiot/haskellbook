module LibForMaybe where

-- exercise 1

isJust :: Maybe a -> Bool
isJust Nothing = False
isJust _ = True

isNothing :: Maybe a -> Bool
isNothing = not . isJust


-- exercise 2
mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee n _ Nothing = n
mayybee _ f (Just a) = f a


-- exercise 3
fromMaybe :: a -> Maybe a -> a
fromMaybe _ (Just a) = a
fromMaybe a Nothing = a

fromMaybe' :: a -> Maybe a -> a
fromMaybe' a ma = mayybee a id ma


-- exercise 4
listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (a:as) = Just a

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just a) = [a]


-- exercise 5
catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (Nothing:mas) = catMaybes mas
catMaybes ((Just a):mas) = a:(catMaybes mas)


-- exercise 6
flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe mas = go (Just []) mas
  where
        go r [] = fmap reverse r 
        go _ (Nothing:mas) = Nothing
        go (Just r) ((Just a):mas) = go (Just (a:r)) mas
