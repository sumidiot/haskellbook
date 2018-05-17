module S11_6 where

-- I'm curious if I can have mutually recursive types
-- My hypothesis is that I can, because haskell is lazy
data T1 = T1 T2
data T2 = T2 T1

a = undefined :: T1
b = T2 a
-- it works!


-- ok, exercises

data Price = Price Integer deriving (Eq, Show)
data Manufacturer = Mini | Mazda | Tata deriving (Eq, Show)
data Airline = PapuAir | CatapultsR'Us | TakeYourChancesUnited deriving (Eq, Show)
data Vehicle = Car Manufacturer Price | Plane Airline deriving (Eq, Show)

myCar = Car Mini (Price 14000)
urCar = Car Mazda (Price 20000)
clownCar = Car Tata (Price 7000)
doge = Plane PapuAir

-- Exercise 1
-- The type of myCar is Vehicle

-- Exercise 2
isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar _ = False

isPlane :: Vehicle -> Bool
isPlane v = case v of
  Plane _ -> True
  otherwise -> False

areCars :: [Vehicle] -> [Bool]
areCars = map isCar

-- Exercise 3
getManu :: Vehicle -> Manufacturer
getManu (Car m _) = m
-- this is a partial function, which is roughly the answer to
-- Exercise 4 - getManu doge fails hard

-- Exercise 5
data Price' = Price' Integer deriving (Eq, Show)
data Size' = Size' Integer deriving (Eq, Show)
data Manufacturer' = Mini' | Mazda' | Tata' deriving (Eq, Show)
data Airline' = PapuAir' | CatapultsR'Us' | TakeYourChancesUnited' deriving (Eq, Show)
data Vehicle' = Car' Manufacturer' Price' | Plane' Airline' Size' deriving (Eq, Show)

-- the only method that needs to change is isPlane
isPlane' :: Vehicle' -> Bool
isPlane' v = case v of
  Plane' _ _ -> True
  otherwise -> False
