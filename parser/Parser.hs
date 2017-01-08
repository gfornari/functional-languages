{-# OPTIONS_GHC -Wall #-}

module Parser where

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
