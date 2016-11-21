toDigitsRev :: Integer -> [Integer]
toDigitsRev digits
    | digits < 10 = [digits]
    | otherwise   = mod digits 10 : toDigitsRev
                    ((digits - digits `mod` 10) `div` 10)

toDigits :: Integer -> [Integer]
toDigits digits = reverse $ toDigitsRev digits

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther digits = case even $ length digits of
    True  -> doubleOdd digits
        where doubleOdd []        = []
              doubleOdd (x:y:xs)  = 2*x : y : doubleOdd xs
    False -> doubleEven digits
        where doubleEven [x]      = [x]
              doubleEven (x:y:xs) = x : 2*y : doubleEven xs

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs)
    | x > 10    = 1 + (x - 10) + sumDigits xs
    | otherwise = x + sumDigits xs

validate :: Integer -> Bool
validate digits
    | (sumDigits $ doubleEveryOther $ toDigits digits) `mod` 10
        == 0    = True
    | otherwise = False
