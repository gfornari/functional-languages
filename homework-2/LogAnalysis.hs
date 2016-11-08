{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import Log

stringToInt :: String -> Int
stringToInt str = read str :: Int

{- It supposes that the first word of an Unknown message does not start with "I", "W" or "E"
otherwise it throws some kind of exception (maybe Exception: Prelude.read: no parse) -}
parseMessage :: String -> LogMessage
parseMessage message = do
    case head tokens of
        "I" -> LogMessage Info (stringToInt (tokens!!1)) (unwords (drop 2 tokens)) 
        "W" -> LogMessage Warning (stringToInt (tokens!!1)) (unwords (drop 2 tokens))
        "E" -> LogMessage (Error (stringToInt (tokens!!1))) (stringToInt (tokens!!2)) (unwords (drop 3 tokens))
        _ -> Unknown message
    where
        tokens = words message

compareTimeStamp :: LogMessage -> LogMessage -> Ordering
compareTimeStamp (LogMessage _ t1 _) (LogMessage _ t2 _) = compare t1 t2

insert :: LogMessage -> MessageTree -> MessageTree
insert logMessage (Leaf) = Node Leaf logMessage Leaf
insert (Unknown _) messageTree = messageTree
insert logMessage (Node minorTree currentLogMessage greaterTree) = do
    case compareTimeStamp logMessage currentLogMessage of
        LT -> insert logMessage minorTree
        _ -> insert logMessage greaterTree

build :: [LogMessage] -> MessageTree
build [] = Leaf
build [a] = insert a Leaf
build logMessages = insert (head logMessages) (build (tail logMessages))

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node minorTree logMessage greaterTree) = inOrder minorTree ++ [logMessage] ++ inOrder greaterTree

parse :: String -> [LogMessage]
parse logs = inOrder (build (map parseMessage (lines logs)))