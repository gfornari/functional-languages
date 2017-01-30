{-# OPTIONS_GHC -Wall #-}

import Control.Applicative
import Data.Char

newtype Parser a = P (String -> [(a, String)])

parse :: Parser a -> String -> [(a, String)]
parse (P p) inp = p inp

instance Functor Parser where
    fmap g p = P (\inp -> case parse p inp of
        [] -> []
        [(v, out)] -> [(g v, out)])

instance Applicative Parser where
    pure v =  P (\inp -> [(v, inp)])
    pg <*> px = P (\inp -> case parse pg inp of
        [] -> []
        [(g, out)] -> parse (fmap g px) out)

instance Monad Parser where
    p >>= f = P (\inp -> case parse p inp of
        [] -> []
        [(v, out)] -> parse (f v) out)

instance Alternative Parser where
    empty = P (\_ -> [])
    p <|> q = P (\inp -> case parse p inp of
        [] -> parse q inp
        [(v, out)] -> [(v, out)])
    many x = some x <|> pure []
    some x = pure (:) <*> x <*> many x


item :: Parser Char
item = P (\inp -> case inp of
    [] -> []
    (x:xs) -> [(x, xs)])

sat :: (Char -> Bool) -> Parser Char
sat p = do
    x <- item
    if (p x) then
        return x
    else
        empty

char :: Char -> Parser Char
char x = sat (==x)

notEOL :: Parser Char
notEOL = sat (/='\n')

string :: String -> Parser String
string [] = return []
string (x:xs) = do
    _ <- char x
    _ <- string xs
    return (x:xs)

space :: Parser ()
space = do
    _ <- many (sat isSpace)
    return ()

token :: Parser a -> Parser a
token p = do
    space
    v <- p
    space
    return v

symbol :: String -> Parser String
symbol xs = token (string xs)

natural :: Parser Int
natural = token nat

nat :: Parser Int
nat = do
    xs <- some digit
    return (read xs)

digit :: Parser Char
digit = sat isDigit

{- Exercise 1. Define a parser comment :: Parser () for ordinary Haskell comments that
begin with -- and extend to the end of the current line, which is represented
by the control character '\n'. -}

comment :: Parser ()
comment = do
    _ <- string "--"
    _ <- many notEOL
    _ <- char '\n'
    return ()


{- Exercise 2. Define a suitable type Expr for arithmetic expressions and modify the
parser for expressions to have expr :: Parser Expr. Hint: Expr should represent
a derivation tree of the input expression. -}

data Expr
    = Nat Int
    | Add Expr Expr
    | Sub Expr Expr
    | Mult Expr Expr
    | Div Expr Expr
    deriving Show

expr :: Parser Expr
expr = do
    t <- term
    do
        _ <- symbol "+"
        e <- expr
        return (Add t e)
        <|> do
            _ <- symbol "-"
            e <- expr
            return (Sub t e)
            <|> return t


term :: Parser Expr
term = do
    f <- factor
    do
        _ <- symbol "*"
        t <- term
        return (Mult f t)
        <|> do
            _ <- symbol "/"
            t <- term
            return (Div f t)
            <|> return f

factor :: Parser Expr
factor = do
    _ <- symbol "("
    e <- expr
    _ <- symbol ")"
    return e
    <|> do
        n <- natural
        return (Nat n)


{- Exercise 3. Consider expressions build up from natural numbers using a subtraction that
is assumed to associate to the left. -}

{- a. Translate this description directly into a grammar.

expr ::= term + expr | expr - term | term
term ::= factor * term | factor
factor ::= (expr) | nat
nat ::= 0 | 1 | 2 | ...
-}

{- b. implement this grammar as a parser expr:: Parser Int. -}

expr3 :: Parser Int
expr3 = do
    t <- term3
    do
        _ <- symbol "+"
        e <- expr3
        return (t + e)
        <|> do
            e <- expr3
            do
                _ <- symbol "-"
                t <- term3
                return (e - t)
                <|> term3


term3 :: Parser Int
term3 = do
    f <- factor3
    do
        _ <- symbol "*"
        t <- term3
        return (f * t)
        <|> return f

factor3 :: Parser Int
factor3 = do
    _ <- symbol "("
    e <- expr3
    _ <- symbol ")"
    return e
    <|> do
        n <- natural
        return n


{- Exercise 4. Define an expression fibs :: [Integer] that generates the
infinite sequence of Fibonacci numbers (0, 1, 1, 2, 3, 5, 8, 13, ...) using
the following simple procedure:
- the first two numbers are 0 and 1;
- the next number is the sum of the previous two;
- return to second step.
Hint: it may be useful to use the functions zip and tail. -}

fibs :: [Integer]
fibs = 0:1:[ x + y | (x,y) <- zip fibs (tail fibs) ]


main :: IO ()
main = do
    print $ parse comment "-- super useful comment\n-- another one!\n"
    print $ parse expr "3*5+8*2/4"
    print $ parse expr3 "8-3-2"
