{-# OPTIONS_GHC -Wall #-}
module Golf where

skips :: [a] -> [[a]]
skips l = zipWith ($) [ skip x | x <- [1..length l]] $ replicate (length l) l
    where skip n l' = [ y | (x,y) <- zip [1..] l', x `mod` n == 0]
