{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import Log

-- Exercise 1

-- I know there are many better ways of doing this, but I think this is enough
-- at this point of the course ;)
parseMessage :: String -> LogMessage
parseMessage str = case words str of 
    ("E":s:ts:msg) -> case (read s :: Int) > 0 && (read ts :: Int) < 101 of
        True -> case (read ts :: Int) > 0 of
            True -> LogMessage (Error (read s)) (read ts) (unwords msg)
            _    -> Unknown str
        _    -> Unknown str
    ("W":ts:msg)   -> case (read ts :: Int) > 0 of
        True -> LogMessage Warning (read ts) (unwords msg)
        _    -> Unknown str
    ("I":ts:msg)   -> case (read ts :: Int) > 0 of
        True -> LogMessage Info (read ts) (unwords msg)
        _    -> Unknown str
    _              -> Unknown str
        
parse :: String -> [LogMessage]
parse fileContents = map parseMessage $ lines fileContents

-- Exercise 2

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert msg         Leaf = Node Leaf msg Leaf
insert _ tree@(Node _ (Unknown _) _) = tree
insert m1@(LogMessage _ ts1 _) (Node left m2@(LogMessage _ ts2 _) right)
    | ts1 > ts2 = Node (insert m1 left) m2 right
    | otherwise = Node left m2 (insert m1 right)

-- Exercise 3

build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

-- Exercise 4

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf                  = []
inOrder (Node left msg right) = inOrder left ++ [msg] ++ inOrder right

-- Exercise 5

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong msgs = map getMsg $ filter important $ sorted msgs
    where sorted = inOrder . build
          important (LogMessage (Error s) _ _) = s >= 50
          important _ = False
          getMsg (LogMessage _ _ msg) = msg
          getMsg _                    = ""
