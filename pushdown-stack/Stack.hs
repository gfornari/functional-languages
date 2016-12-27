{-# OPTIONS_GHC -Wall #-}

module Stack where

type Stack = [Char]

{- From the assignment: "there may be strings that cause a pop with empty
stack. This will raise a system exception. The exercise does not require that
this problem is dealt with." -}
pop :: Stack -> (Char, Stack)
pop [] = error "Empty stack"
pop (x:xs) = (x, xs)

push :: Char -> Stack -> ((), Stack)
push c s = ((), c:s)

{- Checks whether a given string is correctly balanced with respect to
the round parentheses. -}
check_par :: [Char] -> Stack -> Bool
check_par [] [] = True
check_par [] _ = False -- TODO check this one
check_par (c:cs) s = case c of
    '(' -> check_par cs (snd (push ')' s))
    ')' -> check_par cs (snd (pop s))
    _ -> check_par cs s


type Stato = [Char]

newtype ST a = S (Stato -> (a, Stato))

app :: ST a -> Stato -> (a, Stato)
app (S st) x = st x

instance Functor ST where
    fmap g st = S(\s -> let (x, s1) = app st s
                        in (g x, s1))

instance Applicative ST where
    pure x = S(\s -> (x, s))
    stf <*> stx = S(\s -> let (f, s1) = app stf s
                              (x, s2) = app stx s1
                          in (f x, s2))

instance Monad ST where
    return = pure
    st >>= f = S(\s -> let (x, s1) = app st s
                       in app (f x) s1)

pop1 :: ST Char
pop1 = S (\(x:xs) -> (x, xs))

push1 :: ST ()
push1 = S (\s -> ((), ')':xs))

check_par1 :: [Char] -> ST Bool
check_par1 [] = S (\s -> (null s, s))
check_par1 (x:xs) = do
    case x of
        '(' -> do
            push1
            check_par1 xs
        ')' -> do
            _ <- pop1
            check_par1 xs
        _ -> do check_par1 xs

push2 :: Char -> ST ()
push2 a = S (\xs -> ((), a:xs))

check_par2 :: [Char] -> ST Bool
check_par2 [] = S (\s -> (null s, s))
check_par2 (x:xs) = do
    case x of
        '(' -> do
            push2 ')'
            check_par2 xs
        ')' -> do
            _ <- pop1
            check_par2 xs
        _ -> do check_par2 xs