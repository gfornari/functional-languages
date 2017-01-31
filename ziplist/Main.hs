{-# OPTIONS_GHC -Wall #-}

import ZipList

main :: IO ()
main = do
    putStrLn "Showing the Applicative ZipList:"
    print $ Z [(+), (*), (/)] <*> Z [2,4,8] <*> Z [1,3]
    
    putStrLn "Showing the exercise a:"
    print $ zipA [1,2,3] [4,5,6,10] [7,8,9]
    print $ zipA [1,2,3] [] [7,8,9]
    
    putStrLn "Showing the exercise b:"
    print $ zipB [1,2,3] [4,5,6,8] [7,8,9]
    print $ zipB [1,2,3] [] [7,8,9]

    putStrLn "Showing the exercise C:"
    print $ zipC [[1,2],[3,4],[5,6]]
