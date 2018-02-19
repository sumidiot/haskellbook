{-# LANGUAGE NoMonomorphismRestriction #-}

module ChExDtT where

a = (* 9) 6 -- without the {-#, this is Integer

b = head [(0, "doge"), (1, "kitteh")] -- without the {-#, this is (Integer, [Char])

c = head [(0 :: Integer, "doge"), (1, "kitteh")]

d = if False then True else False

e = length [1, 2, 3, 4, 5]

f = (length [1, 2, 3, 4]) > (length "TACOCAT")

x2 = 5
y2 = x2 + 5
w2 = y2 * 10

x3 = 5
y3 = x3 + 5
z3 y = y * 10

x4 = 5
y4 = x4 + 5
f4 = 4 / y4

x5 = "Julia"
y5 = " <3 "
z5 = "Haskell"
f5 = x5 ++ y5 ++ z5



