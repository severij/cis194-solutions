type Peg = String
type Move = (Peg, Peg)

-- 3 Pegs, a c b
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 1 a b _ = [(a, b)]
hanoi n a b c = hanoi (n-1) a c b ++ [(a, b)] ++ hanoi (n-1) c b a

-- 4 Pegs, a c d b (Optional)
hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 1 a b _ _ = [(a, b)]
hanoi4 2 a b _ d = [(a, d), (a, b), (d, b)]
hanoi4 n a b c d = hanoi4 (n-2) a c b d ++ [(a, d), (a, b), (d, b)] ++
                   hanoi4 (n-2) c b a d
