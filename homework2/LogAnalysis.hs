{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import Log

-- Exercise 1

-- I know there are many better ways of doing this, but I think this is enough
-- at this point of the course ;)
parseMessage :: String -> LogMessage
parseMessage str = check $ words str
    where
        check (x:y:z:xs) = case x of
                "E" -> case (ry > 0 && ry < 101) of
                    True -> case rz > 0 of
                        True -> LogMessage (Error ry) rz (unwords xs)
                        _    -> Unknown str
                    _    -> Unknown str
                "W" -> case ry > 0 of
                    True -> LogMessage Warning ry (z ++ " " ++ unwords xs)
                    _    -> Unknown str
                "I" -> case ry > 0 of
                    True -> LogMessage Info ry (z ++ " " ++ unwords xs)
                    _    -> Unknown str
                _   -> Unknown str
            where ry = read y :: Int
                  rz = read z :: Int
        check _          = Unknown str
        
parse :: String -> [LogMessage]
parse fileContents = map parseMessage $ lines fileContents

-- Exercise 2

getTS :: LogMessage -> Int
getTS (LogMessage (Error _) ts _) = ts
getTS (LogMessage Warning   ts _) = ts
getTS (LogMessage Info      ts _) = ts
getTS (Unknown                 _) = 0

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree                   = tree
insert msg         Leaf                   = Node Leaf msg Leaf
insert msg         (Node left msg2 right) = case getTS msg < getTS msg2 of
                                                True  -> insert msg left
                                                False -> insert msg right

-- Exercise 3

build :: [LogMessage] -> MessageTree
build msgs = let build' [] tree = tree
                 build' (x:xs) tree = build' xs (insert x tree)
             in build' msgs Leaf

-- Exercise 4

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf                  = []
inOrder (Node Leaf msg Leaf ) = [msg]
inOrder (Node left msg Leaf ) = inOrder left ++ [msg]
inOrder (Node Leaf msg right) = [msg] ++ inOrder right
inOrder (Node left msg right) = inOrder left ++ [msg] ++ inOrder right

-- Exercise 5

getMsg :: LogMessage -> String
getMsg (LogMessage (Error _) _ msg) = msg
getMsg (LogMessage Warning   _ msg) = msg
getMsg (LogMessage Info      _ msg) = msg
getMsg (Unknown                _  ) = []

getErrS :: LogMessage -> Int
getErrS (LogMessage (Error s) _ _) = s
getErrS _                          = -1

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong msgs = map getMsg (filter ((>= 50) . getErrS) msgs)
