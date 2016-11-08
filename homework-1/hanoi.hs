{-# OPTIONS_GHC -Wall #-}

type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n a b c = hanoi (n-1) a c b ++ [(a,b)] ++ hanoi (n-1) c b a

main :: IO ()
main = do
    putStrLn "Executing hanoi..."
    putStrLn (show (hanoi 1 "a" "b" "c"))
    putStrLn (show (hanoi 2 "a" "b" "c"))
    putStrLn (show (hanoi 3 "a" "b" "c"))
    putStrLn (show (hanoi 4 "a" "b" "c"))