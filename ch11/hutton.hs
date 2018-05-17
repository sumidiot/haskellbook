module Hutton where

data Expr = Lit Integer | Add Expr Expr

-- Exercise 1
eval :: Expr -> Integer
eval (Lit i) = i
eval (Add l r) = (eval l) + (eval r)

-- Exercise 2
printExpr :: Expr -> String
printExpr (Lit i) = show i
printExpr (Add l r) = (printExpr l) ++ " + " ++ (printExpr r)

