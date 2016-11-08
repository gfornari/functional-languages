{-# OPTIONS_GHC -Wall #-}

import System.IO


getCh :: IO Char
getCh = do
    hSetEcho stdin False
    x <- getChar
    hSetEcho stdin True
    return x

sgetLine :: IO String
sgetLine = do
    x <- getCh
    if x == '\n' then do
        putChar x
        return []
    else do
        putChar '-'
        xs <- sgetLine
        return (x:xs)

filterRightChars :: String -> String -> String
filterRightChars [] _ = []
filterRightChars (w:ws) guess = do
    if elem w guess then do
        [w] ++ filterRightChars ws guess
    else do
        ['-'] ++ filterRightChars ws guess

play :: String -> IO ()
play word = do
    guess <- getLine
    if guess == word then
        putStrLn "You got it!!"
    else do
        putStrLn (filterRightChars word guess)
        play word

hangman :: IO ()
hangman = do
    putStrLn "Think a word: "
    word <- sgetLine
    putStrLn "Try to guess it: "
    play word

main :: IO ()
main = hangman