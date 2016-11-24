{-# OPTIONS_GHC -Wall #-}
module Golf where

skips :: [a] -> [[a]]
skips l = zipWith ($) [ skip x | x <- [1..length l]] $ replicate (length l) l
    where skip n l' = [ y | (x,y) <- zip [1..] l', x `mod` n == 0]

localMaxima :: [Integer] -> [Integer]
localMaxima l = [ l !! n | n <- [0..(length l - 2)], l !! n > l !! (n + 1)]

histogram :: [Integer] -> String
histogram l = unlines (map change (chop (count l))) ++"==========\n0123456789\n"
    where count a  = [ length (filter (== n) a) | n <- [0..9]]
          chop b   = reverse $ take (maximum b) $ iterate (map pred) b
          change c = map (\x -> if x > 0 then '*' else ' ') c
