{-# OPTIONS_GHC -Wall #-}

import ZipList

main :: IO ()
main = do
    putStrLn "Showing the Applicative ZipList:"
    putStrLn (show (Z [(+), (*), (/)] <*> Z [2,4,8] <*> Z [1,3]))
    
    putStrLn "Showing the exercise a:"
    putStrLn (show (zipA [1,2,3] [4,5,6,10] [7,8,9]))
    putStrLn (show (zipA [1,2,3] [] [7,8,9]))
    
    putStrLn "Showing the exercise b:"
    putStrLn (show (zipB [1,2,3] [4,5,6,8] [7,8,9]))
    putStrLn (show (zipB [1,2,3] [] [7,8,9]))

    putStrLn "Showing the exercise C:"
    putStrLn (show (zipC [[1,2],[3,4],[5,6]]))
