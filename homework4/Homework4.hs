{-# OPTIONS_GHC -Wall #-}
import Data.List

fun1 :: [Integer] -> Integer
fun1 = foldr (*) 1 . map (pred . pred) . filter even

fun2 :: Integer -> Integer
fun2 = sum . filter even . takeWhile (1 <) . iterate (\x ->
    if even x
        then x `div`2
        else 3 * x + 1)

data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
            deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree = foldr insert' Leaf
    where insert' x Leaf = Node 0 Leaf x Leaf
          insert' x (Node h l y r)
            | f l > f r = Node h l y (insert' x r)
            | f l < f r = Node h (insert' x l) y r
            | otherwise = Node (1 + max (f r) (f (insert' x l))) (insert' x l) y r
                where f Leaf             = -1
                      f (Node f' _ _ _) = f'

xor :: [Bool] -> Bool
xor = foldr1 (/=)

map' :: (a -> b) -> [a] -> [b]
map' f = foldr ((:) . f) []

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base = foldr (flip f) base . reverse

cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]

sieveSundaram :: Integer -> [Integer]
sieveSundaram = map (\x -> 2*x + 1) . (\n -> [1..n] \\ [i+j+2*i*j| i <- [1..n],
        j <- [1..n], 1 <= i && i <= j, i+j+2*i*j <= n])
