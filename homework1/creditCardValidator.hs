{-# OPTIONS_GHC -Wall #-}

-- Exercise 1
toDigitsRev :: Integer -> [Integer]
toDigitsRev digits
    | digits < 10 = [digits]
    | otherwise   = mod digits 10 : toDigitsRev
                    ((digits - digits `mod` 10) `div` 10)

toDigits :: Integer -> [Integer]
toDigits digits = reverse $ toDigitsRev digits

-- Exercise 2
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther digits = case even $ length digits of
    True  -> doubleOdd digits
        where doubleOdd []        = []
              doubleOdd [x]       = [2*x]
              doubleOdd (x:y:xs)  = 2*x : y : doubleOdd xs
    False -> doubleEven digits
        where doubleEven []       = []
              doubleEven [x]      = [x]
              doubleEven (x:y:xs) = x : 2*y : doubleEven xs

-- Exercise 3
sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs)
    | x > 10    = 1 + (x - 10) + sumDigits xs
    | otherwise = x + sumDigits xs

-- Exercise 4
validate :: Integer -> Bool
validate digits
    | (sumDigits $ doubleEveryOther $ toDigits digits) `mod` 10 == 0 = True
    | otherwise                                                      = False
