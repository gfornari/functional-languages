{-# OPTIONS_GHC -Wall #-}

import Stack

main :: IO ()
main = do
    putStrLn ("check_par \"a((cs)cs)\" \"\" --> " ++ show (check_par "a((cs)cs)" ""))
    putStrLn ("check_par \"a((cscs)\" \"\" --> " ++ show (check_par "a((cscs)" ""))
    putStrLn ("app (check_par1 \"a((cs)cs)\") \"\" --> " ++ show (app (check_par1 "a((cs)cs)") ""))
    putStrLn ("app (check_par1 \"a((cscs)\") \"\" --> " ++ show (app (check_par1 "a((cscs)") ""))
    putStrLn ("app (check_par2 \"a((cs)cs)\") \"\" --> " ++ show (app (check_par2 "a((cs)cs)") ""))
    putStrLn ("app (check_par2 \"a((cscs)\") \"\" --> " ++ show (app (check_par2 "a((cscs)") ""))