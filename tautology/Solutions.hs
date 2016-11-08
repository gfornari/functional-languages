{-# OPTIONS_GHC -Wall #-}

module Solutions(vars, ttables, solutions, Prop(..)) where

data Prop = Const Bool
          | Var Char
          | Not Prop
          | And Prop Prop
          | Imply Prop Prop
          deriving (Show)

{- The unique function removes duplicate elements from a
list keeping the first occurrence of each element. -}
unique :: Eq a => [a] -> [a]
unique [] = []
unique (x:xs) = x : unique (filter (/= x) xs)

{- The booleans function returns a list of list of the
given size. Each list is a combination of True and False. -}
booleans :: Int -> [[Bool]]
booleans 0 = [[]]
booleans n = map (True:) b ++ map (False:) b
    where b = booleans (n - 1)

{- The firstEqual function returns whether if the first element
of a tuple is equal to a given value. -}
firstEqual :: Eq a => a -> (a, b) -> Bool
firstEqual a b = (fst b) == a

{- The find function returns the first occurrence in a list
of tuples where a value is equal to the first element of the
tuple. -}
find :: Eq a => a -> [(a, b)] -> b
find a list = snd (head (filter (firstEqual a) list))

{- The eval function evaluates a proposition with a list of
assignments. -}
eval :: Prop -> [(Char,Bool)] -> Bool
eval (Const b) _ = b
eval (Var x) s = find x s
eval (Not p) s = not (eval p s)
eval (And p q) s = eval p s && eval q s
eval (Imply p q) s = eval p s <= eval q s

{- The vars function returns the list of the propositional
variables contained in a proposition. It keeps the order
of occurrence and takes only the unique occurrences. -}
vars :: Prop -> [Char]
vars (Const _) = []
vars (Var c) = [c]
vars (Not p) = vars p
vars (And p1 p2) = unique (vars p1 ++ vars p2)
vars (Imply p1 p2) = unique (vars p1 ++ vars p2)

{- The ttables function returns every assignments of truth
values for a list of char. -}
ttables :: [Char] -> [[(Char,Bool)]]
ttables cs = map (zip cs) (booleans (length cs))

{- The solutions function checks whether a proposition is a
tautology or not. -}
solutions :: Prop -> [[(Char,Bool)]] -> Bool
solutions p tt = and (map (eval p) tt)
