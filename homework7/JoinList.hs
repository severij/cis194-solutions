{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
import Data.Monoid ((<>))
import Sized
import Scrabble
import Buffer
import Editor (runEditor, editor)

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
                    deriving (Eq, Show)

-- Exercise 1
tag :: Monoid m => JoinList m a -> m
tag Empty          = mempty
tag (Single t _)   = t
tag (Append t _ _) = t

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) a b = Append (tag a <> tag b) a b

-- Exercise 2
indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ i jl
    | i >= 0 =
        case jl of
            Single _ a                   -> Just a
            Append _ jl1 jl2
                | size' > i -> indexJ i jl1
                | otherwise -> indexJ (i - size') jl2
                where size' = getSize (size (tag jl1))
    | otherwise = Nothing

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ n jl
    | n >= 0 =
        case jl of
            Empty                         -> Empty
            Single _ _                    -> Empty
            Append _ jl1 jl2
                | size' == n -> jl2
                | size' <  n -> dropJ (n - size') jl2
                | otherwise  -> Append tag' jl1' jl2
                where size' = getSize (size (tag jl1))
                      jl1'  = dropJ n jl1
                      tag'  = tag jl1' <> tag jl2
    | otherwise = jl

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ n jl
    | n >= 0 =
        case jl of
            Single t a | n >= 1 -> Single t a
            Append _ jl1 jl2
                | size' == n    -> jl1
                | size' >  n    -> takeJ n jl1
                | otherwise     -> Append tag' jl1 jl2'
                where size' = getSize (size (tag jl1))
                      jl2'  = takeJ (n - size') jl2
                      tag'  = tag jl1 <> tag jl2'
    | otherwise = Empty

-- Exercise 3
scoreLine :: String -> JoinList Score String
scoreLine []  = Empty
scoreLine str = Single (scoreString str) str

-- Exercise 4
instance Buffer (JoinList (Score, Size) String) where

    toString Empty              = ""
    toString (Single _ str)     = str
    toString (Append _ jl1 jl2) = toString jl1 ++ toString jl2

    fromString []     = Empty
    fromString (x:[]) = Single (score x, 1) (x:[])
    fromString str    = Single (scoreString str, 1) str
    
    line = indexJ

    replaceLine i ln jl = takeJ i jl +++ newJl +++ dropJ (i+1) jl
      where
        newJl = Single (scoreString ln, 1) ln

    numLines jl = getSize(size (tag jl))

    value Empty                          = 0
    value (Single (Score score', _) _)   = score'
    value (Append (Score score', _) _ _) = score'

-- TODO
main = runEditor editor (JoinList (Score 0, Size 0) [])

-- Functions that will come in handy when checking the correctness of the
-- solutions.
(!!?) :: [a] -> Int -> Maybe a
[] !!? _        = Nothing
_ !!? i | i < 0 = Nothing
(x:xs) !!? 0    = Just x
(x:xs) !!? i    = xs !!? (i-1)

jlToList :: JoinList m a -> [a]
jlToList Empty            = []
jlToList (Single _ a)     = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2
