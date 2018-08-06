module HeavyLifting where

-- exercise 5
e :: IO Integer
e = let ioi = readIO "1" :: IO Integer
        changed = read <$> ("123"++) <$> (show <$> ioi)
    in (*3) <$> changed

