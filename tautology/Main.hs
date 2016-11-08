{-# OPTIONS_GHC -Wall #-}

import Solutions

showResults :: Prop -> IO ()
showResults p = do
    let x = vars p
        y = ttables x
        z = solutions p y
    putStrLn (show  x)
    putStrLn (show  y)
    putStrLn (show  z)

main :: IO ()
main = do
    showResults (And (Var 'A') (Not (Var 'A')))
    showResults (And (Const True) (Imply (And (Var 'A') (Var 'B')) (Var 'A')))
    showResults (And (Imply (Var 'A') (And (Var 'A') (Var 'B'))) (Const False))
    showResults (Not (Imply (And (Var 'A') (Imply (Var 'A') (Var 'B'))) (Var 'B')))