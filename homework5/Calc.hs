{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

import ExprT
import Parser
import qualified StackVM as S
import qualified Data.Map.Strict as M

-- Exercise 1
eval :: ExprT -> Integer
eval (Lit n)     = n
eval (Add l1 l2) = eval l1 + eval l2
eval (Mul l1 l2) = eval l1 * eval l2

-- Exercise 2
evalStr :: String -> Maybe Integer
evalStr str = case parseExp Lit Add Mul str of
                  Just expr -> Just (eval expr)
                  Nothing   -> Nothing

-- Exercise 3
class Expr a where
    lit :: Integer -> a
    add :: a -> a -> a
    mul :: a -> a -> a

instance Expr ExprT where
    lit = Lit
    add = Add
    mul = Mul

-- Exercise 4
newtype MinMax = MinMax Integer deriving (Eq, Show)
newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr Integer where
    lit = id
    add = (+)
    mul = (*)

instance Expr Bool where
    lit = (0 <)
    add = (||)
    mul = (&&)

instance Expr MinMax where
    lit n = MinMax n
    add (MinMax l1) (MinMax l2) = MinMax (max l1 l2)
    mul (MinMax l1) (MinMax l2) = MinMax (min l1 l2)

instance Expr Mod7 where
    lit n = Mod7 (mod n 7)
    add (Mod7 l1) (Mod7 l2) = Mod7 (mod (l1 + l2) 7)
    mul (Mod7 l1) (Mod7 l2) = Mod7 (mod (l1 * l2) 7)

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger = testExp :: Maybe Integer
testBool = testExp :: Maybe Bool
testMM = testExp :: Maybe MinMax
testSat = testExp :: Maybe Mod7

-- Exercise 5
instance Expr S.Program where
    lit n = [S.PushI n]
    add l1 l2 = l1 ++ l2 ++ [S.Add]
    mul l1 l2 = l1 ++ l2 ++ [S.Mul]

compile :: String -> Maybe S.Program
compile = parseExp lit add mul

-- Exercise 6
class HasVars a where
    var :: String -> a

data VarExprT = VLit Integer
              | VAdd VarExprT VarExprT
              | VMul VarExprT VarExprT
              | VVar String deriving (Eq, Show)

instance Expr VarExprT where
    lit = VLit
    add = VAdd
    mul = VMul

instance HasVars VarExprT where
    var = VVar

instance HasVars (M.Map String Integer -> Maybe Integer) where
    var = M.lookup

instance Expr (M.Map String Integer -> Maybe Integer) where
    lit n = \_ -> Just n
    add a b m = do n1 <- a m
                   n2 <- b m
                   return (n1 + n2)
    mul a b m = do n1 <- a m
                   n2 <- b m
                   return (n1 * n2)

withVars :: [(String, Integer)]
    -> (M.Map String Integer -> Maybe Integer) -> Maybe Integer
withVars vs exp = exp $ M.fromList vs
