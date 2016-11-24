{-# OPTIONS_GHC -Wall #-}

import ExprT
import Parser

eval :: ExprT -> Integer
eval (Lit n)     = n
eval (Add l1 l2) = eval l1 + eval l2
eval (Mul l1 l2) = eval l1 * eval l2

evalStr :: String -> Maybe Integer
evalStr str = case parseExp Lit Add Mul str of
                  Just expr -> Just (eval expr)
                  Nothing   -> Nothing

{-class Expr where-}
    {-lit :: Integer-}
    {-add :: Expr -> Expr -> Integer-}
    {-mul ::-}
