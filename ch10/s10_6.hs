module S10_6 where

import Data.Time

data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate   UTCTime
                  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
  [
    DbDate (UTCTime (fromGregorian 1911 5 1) (secondsToDiffTime 34123)),
    DbNumber 9001,
    DbString "Hello, World!",
    DbDate (UTCTime (fromGregorian 1921 5 1) (secondsToDiffTime 34123))
  ]

-- exercise 1
filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate = foldr f [] -- point-free (pointless) style!
  where f i ts = case i of
                 DbDate t -> t:ts
                 _        -> ts


-- exercise 2
filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber = foldr f [] -- point-free (pointless) style!
  where f i is = case i of
                 DbNumber n -> n:is
                 _          -> is


filterAsFold :: (a -> Bool) -> [a] -> [a]
filterAsFold p = foldr f [] where f a l = if p a then a:l else l

isDbNumber :: DatabaseItem -> Bool
isDbNumber (DbNumber _) = True
isDbNumber _            = False

-- ah, i want to be able to do the following, but there's no type constructor for DbNumber, just a data constructor
--unpackDbNumber :: DbNumber -> Integer
--unpackDbNumber DbNumber n = n

--filterDbNumber' :: [DatabaseItem] -> [Integer]
--filterDbNumber' db = map unpackDbNumber (filterAsFold isDbNumber db)


-- exercise 3
mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = foldr f minTime
  where minTime = UTCTime (fromGregorian 0 0 0) (secondsToDiffTime 0)
        f (DbDate d) t = max d t
        f _          t = t


-- exercise 4
sumDb :: [DatabaseItem] -> Integer
sumDb = foldr f 0
  where f (DbNumber n) s = n + s
        f _            s = s


-- exercise 5
avgDb :: [DatabaseItem] -> Double
avgDb db = (fromIntegral s) / (fromIntegral c)
  where (s,c) = foldr f (0,0) db
                where f (DbNumber d) (s,c) = (s+d,c+1)
                      f _            (s,c) = (s,c)

