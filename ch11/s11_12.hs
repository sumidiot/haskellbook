module S11_12 where

data FlowerType = Gardenia | Daisy | Rose | Lilac deriving Show

type Gardener = String

data Garden = Garden Gardener FlowerType deriving Show

-- Exercise 1: what is the sum of products normal form of Garden?
-- Note that Garden is the product Gardener * FlowerType, or
-- Gardener * (Gardenia | Daisy | Rose | Lilac), which distributes as
-- (Gardener * Gardenia) | (Gardener * Daisy) | (Gardener * Rose) | (Gardener * Lilac)

-- using the same re-write as they did for Author, I think the answer could be
data Garden' = Gardenia' Gardener | Daisy' Gardener | Rose' Gardener | Lilac' Gardener
