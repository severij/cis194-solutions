{-# OPTIONS_GHC -Wall #-}

import ExprT

eval :: ExprT -> Integer
eval (Lit n)     = n
eval (Add l1 l2) = eval l1 + eval l2
eval (Mul l1 l2) = eval l1 * eval l2
