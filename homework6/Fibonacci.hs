{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

-- Exercise 1
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibs1 :: [Integer]
fibs1 = [fib n | n <- [1..]]

-- Exercise 2
fibs2 :: [Integer]
fibs2 =  let fib2 a b = (a + b) : fib2 b (a + b) in 0 : 1 : fib2 0 1

-- Exercise 3
data Stream a = Stream a (Stream a)

instance Show a => Show (Stream a) where
    show stream = showN 20 1 stream where
        showN n c (Stream v s)
            | c < n     = show v ++ ", " ++ showN n (c+1) s
            | otherwise = show v

streamToList :: Stream a -> [a]
streamToList (Stream v s) = v : streamToList s

-- Exercise 4
streamRepeat :: a -> Stream a
streamRepeat x = Stream x (streamRepeat x)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Stream v s) = Stream (f v) (streamMap f s)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f s = Stream s (streamFromSeed f (f s))

-- Exercise 5
nats :: Stream Integer
nats = streamFromSeed (+ 1) 1

ruler :: Stream Integer
ruler = streamMap largestPowerOf2 nats where
    largestPowerOf2 n = largest 0 n 0 where
        largest l n i = case n < 2^i of
                            True -> l
                            False -> case n `mod` 2^i == 0 of
                                True  -> largest i n (i + 1)
                                False -> largest l n (i + 1)

-- Exercise 6
x :: Stream Integer
x = Stream 0 $ Stream 1 $ streamRepeat 0

instance Num (Stream Integer) where
    fromInteger i = Stream i $ streamRepeat 0
    negate (Stream v s) = Stream (-v) $ negate s
    (+) (Stream v1 s1) (Stream v2 s2) = Stream (v1 + v2) (s1 + s2)
    (*) (Stream v1 s1) (Stream v2 s2) = Stream (v1 * v2)
        (streamMap (* v1) s2 + s1 * Stream v2 s2)

instance Fractional (Stream Integer) where
    (/) (Stream v1 s1) (Stream v2 s2) = q where
        first   = v1 `div` v2
        divider = \x -> x `div` v2
        remainder = s1 - q * s2
        q = Stream first (streamMap divider remainder)

fibs3 :: Stream Integer
fibs3 = x / (1 - x - x^2)

-- Exercise 7
data Matrix a = Matrix ((a, a), (a, a))

instance Num (Matrix Integer) where
    (Matrix ((a, b), (c, d))) * (Matrix ((e, f), (g, h))) = Matrix
        ((a * e + b * g, a * f + b * h),
        (c * e + d * g, c * f + d * h))

fib4 :: Integer -> Integer
fib4 0 = 1
fib4 n = f ((Matrix ((1, 1), (1, 0)))^n) where f (Matrix ((a,_), _)) = a
         
