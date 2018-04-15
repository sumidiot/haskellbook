module S9_7 where -- List comprehensions


-- Comprehend Thy List
mySqr = [x^2 | x <- [1..10]]

ctl1 = [x | x <- mySqr, rem x 2 == 0]
ctl2 = [(x, y) | x <- mySqr, y <- mySqr, x < 50, y > 50]
ctl3 = take 5 [ (x, y) | x <- mySqr, y <- mySqr, x < 50, y > 50]


-- Square Cube
mySqr2 = [x^2 | x <- [1..5]]
myCube = [y^3 | y <- [1..5]]

tuples = [(x, y) | x <- mySqr2, y <- myCube]
tuples2 = [(x, y) | x <- mySqr2, x < 50, y <- myCube, y < 50] -- note you can mix a predicate between generators
tupLen = length tuples2
