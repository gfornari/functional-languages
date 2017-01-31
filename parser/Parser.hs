{-# OPTIONS_GHC -Wall #-}

module Parser where

import Control.Applicative
import Data.Char

data LKC
    = ADD       LKC LKC
    | CALL      LKC [LKC]
    | CONS      LKC LKC
    | DIV       LKC LKC
    | EQ        LKC LKC
    | H         LKC
    | IF        LKC LKC LKC
    | LAMBDA    [LKC] LKC
    | LEQ       LKC LKC
    | LET       LKC [(LKC,LKC)]
    | LETREC    LKC [(LKC,LKC)]
    | MULT      LKC LKC
    | NUM       Int
    | NULL
    | SUB       LKC LKC
    | T         LKC
    | VAR       String
    deriving (Show, Eq)

keywords :: [String]
keywords = ["let", "letrec", "in", "end", "and", "if", "then", "else", "lambda", "null", "cons", "head", "tail", "eq", "leq"]

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

digit :: Parser Char
digit = sat isDigit

lower :: Parser Char
lower = sat isLower

upper :: Parser Char
upper = sat isUpper

letter :: Parser Char
letter = sat isAlpha

alphanum :: Parser Char
alphanum = sat isAlphaNum

char :: Char -> Parser Char
char x = sat (==x)

string :: String -> Parser String
string [] = return []
string (x:xs) = do
    _ <- char x
    _ <- string xs
    return (x:xs)

ident :: Parser String
ident = do
    x <- lower
    xs <- many alphanum
    return (x:xs)

nat :: Parser Int
nat = do
    xs <- some digit
    return (read xs)

space :: Parser ()
space = do
    _ <- many (sat isSpace)
    return ()

int :: Parser Int
int = do
    _ <- char '-'
    n <- nat
    return (-n)
    <|> nat

token :: Parser a -> Parser a
token p = do
    space
    v <- p
    space
    return v

identifier :: Parser String
identifier = token ident

natural :: Parser Int
natural = token nat

integer :: Parser Int
integer = token int

symbol :: String -> Parser String
symbol xs = token (string xs)

nats :: Parser [Int]
nats = do
    _ <- symbol "["
    n <- natural
    ns <- many (do
        _ <- symbol ","
        natural)
    _ <- symbol "]"
    return (n:ns)


{- ***** LKC parsers ***** -}


var :: Parser String
var = do
    s <- identifier
    if s `elem` keywords then
        empty
    else
        return s


seqVar :: Parser [LKC]
seqVar = do
    vs <- many var
    return (map (VAR) vs)


prog :: Parser LKC
prog = do
    _ <- symbol "let"
    b1 <- binds
    _ <- symbol "in"
    e1 <- expr
    _ <- symbol "end"
    return (LET e1 b1)
    <|> do
        _ <- symbol "letrec"
        b2 <- binds
        _ <- symbol "in"
        e2 <- expr
        _ <- symbol "end"
        return (LETREC e2 b2)


bind :: Parser (LKC, LKC)
bind = do
    v <- var
    _ <- symbol "="
    e <- expr
    return (VAR v, e)



binds :: Parser [(LKC, LKC)]
binds = do
    b <- bind
    bs <- many (do
        _ <- symbol "and"
        bind)
    return (b:bs)


{- Watch out the order -}
expr :: Parser LKC
expr = prog <|> lambda <|> prefixop <|> ifthenelse <|> expa


lambda :: Parser LKC
lambda = do
    _ <- symbol "lambda"
    _ <- symbol "("
    vs <- seqVar
    _ <- symbol ")"
    e <- expr
    return (LAMBDA vs e)


expa :: Parser LKC
expa = do
    t <- term
    _ <- symbol "+"
    e <- expa
    return (ADD t e)
    <|> do
        t <- term
        _ <- symbol "-"
        e <- expa
        return (SUB t e)
    <|> term
    

term :: Parser LKC
term = do
    f <- factor
    _ <- symbol "*"
    t <- term
    return (MULT f t)
    <|> do
        f <- factor
        _ <- symbol "/"
        t <- term
        return (MULT f t)
    <|> factor


seq_expr :: Parser [LKC]
seq_expr = do
    e <- expr
    es <- many (do
        _ <- symbol ","
        expr)
    return (e:es)


factor :: Parser LKC
factor = do
    v <- var
    _ <- symbol "("
    _ <- symbol ")"
    return (CALL (VAR v) [])
    <|> do
        v <- var
        _ <- symbol "("
        es <- seq_expr
        _ <- symbol ")"
        return (CALL (VAR v) es)
    <|> do
        v <- var
        return (VAR v)
    <|> do
        n <- integer
        return (NUM n)
    <|> do
        _ <- symbol "null"
        return (NULL)
    <|> do
        _ <- symbol "("
        e <- expa
        _ <- symbol ")"
        return e


ifthenelse :: Parser LKC
ifthenelse = do
    _ <- symbol "if"
    e1 <- expr
    _ <- symbol "then"
    e2 <- expr
    _ <- symbol "else"
    e3 <- expr
    return (IF e1 e2 e3)


prefixop :: Parser LKC
prefixop = do
    _ <- symbol "cons"
    _ <- symbol "("
    e0 <- expr
    _ <- symbol ","
    e1 <- expr
    _ <- symbol ")"
    return (CONS e0 e1)
    <|> do
        _ <- symbol "head"
        _ <- symbol "("
        e <- expr
        _ <- symbol ")"
        return (H e)
    <|> do
        _ <- symbol "tail"
        _ <- symbol "("
        e <- expr
        _ <- symbol ")"
        return (T e)
    <|> do
        _ <- symbol "eq"
        _ <- symbol "("
        e0 <- expr
        _ <- symbol ","
        e1 <- expr
        _ <- symbol ")"
        return (Parser.EQ e0 e1)
    <|> do
        _ <- symbol "leq"
        _ <- symbol "("
        e0 <- expr
        _ <- symbol ","
        e1 <- expr
        _ <- symbol ")"
        return (LEQ e0 e1)


test1 :: String
test1 = "let x=2 and y=4 in x+y*2 end"

test2 :: String
test2 = "letrec fact = lambda (n) if eq(n,1) then 1 else n* fact (n-1) and x=cons(1, cons( 2, null)) and f = lambda (l g) if eq(l, null) then null else cons(g (head( l)) , f (g, tail (l))) in f(x,fact) end"

printTest :: String -> IO ()
printTest test = do
    putStr "\n"
    putStrLn test
    putStr "\n"
    print $ parse prog test

main :: IO ()
main = do
    printTest test1
    printTest test2