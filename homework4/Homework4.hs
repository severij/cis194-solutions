{- GHC_OPTION -Wall -}

fun1 :: [Integer] -> Integer
fun1 = foldr (*) 1 . map (pred . pred) . filter even

fun2 :: Integer -> Integer
fun2 = sum . filter even . takeWhile (1 <) . iterate (\x ->
    if even x
        then x `div`2
        else 3 * x + 1)
