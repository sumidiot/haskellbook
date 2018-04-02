{- 7.9 -}

-- No exercise section.
-- Here are the functions to study:

add x y = x + y
addPF = (+)
addOne = \x -> x + 1
addOnePF = (+1)

-- add: add two parameters together
-- addPF: equivalent to addition (non-infix)
-- addOne: add 1 to argument
-- addOnePF: partially applied 
--             i.e., similar to  \x -> (+) 1 x  

main = do
    print (0 :: Int)
    print (add 1 0)
    print (addOne 0)
    print (addOnePF 0)
    -- 0 -> addOne -> 1 -> addOne -> 2
    print ((addOne . addOne) 0)
    -- 0 -> addOne -> 1 -> addOnePF -> 2
    print ((addOnePF . addOne) 0)
    -- 0 -> addOnePF -> 1 -> addOne -> 2 
    print ((addOne . addOnePF) 0)
    -- 0 -> 1 -> 2
    print ((addOnePF . addOnePF) 0)
    -- 0 -> 1 -> -1
    print (negate (addOne 0))
    -- 0 -> -1
    print ((negate . addOne) 0)
    -- 0 -> (1 -> -1 -> 0 -> 1 -> 2) -> 2
    --      ^^^^ effect of composed function
    print ((addOne . addOne . addOne 
             . negate . addOne) 0)

